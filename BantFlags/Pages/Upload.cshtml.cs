using BantFlags.Data;
using BantFlags.Data.Database;
using ImageMagick;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags
{
    // I don't know if I need these anymore.
    [RequestFormLimits(ValueCountLimit = 5000)]
    [IgnoreAntiforgeryToken(Order = 2000)]
    public class UploadModel : PageModel
    {
        private IWebHostEnvironment Env { get; }
        private DatabaseService Database { get; set; }

        private readonly byte[] PNGHeader = new byte[] { 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A };

        private string FlagsPath { get; set; }

        public Staging staging { get; set; }

        public UploadModel(IWebHostEnvironment env, DatabaseService db, Staging s)
        {
            Env = env;
            Database = db;

            staging = s;

            FlagsPath = Env.WebRootPath + "/flags/";
        }

        public string Message { get; private set; }

        // TODO: These bound properties should be inlined.
        [BindProperty]
        public IFormFile Upload { get; set; }

        [BindProperty]
        public bool ShouldGloss { get; set; }

        public async void OnGet()
        {
            if (staging.Flags == null)
            {
                staging.Flags = await Database.GetFlags(); // Because we can't populate Flags in the constructor.
            }
        }

        public IActionResult OnPostUnstage(List<FormFlag> addedAndDeletedFlags, List<RenameFlag> renamedFlags, string password)
        {
            if (password != staging.Password) // TODO: Maybe we should hash this?
            {
                Message = "Wrong Password";
                return Page();
            }

            try // Haha I can't program
            {
                var addedAndDeleted = addedAndDeletedFlags.Where(x => x.IsChecked);
                var renamed = renamedFlags.Where(x => x.IsChecked);

                addedAndDeleted.ForEach(x =>
                    _ = x.FormMethod switch
                    {
                        // Using an enum seems kinda redundant here.
                        Method.Delete => staging.DeletedFlags.Remove(staging.DeletedFlags.First(y => y.Name == x.Name)),
                        Method.Add => staging.AddedFlags.Remove(staging.AddedFlags.First(y => y.Name == x.Name)),
                        _ => throw new Exception()
                    });

                renamed.ForEach(x => staging.RenamedFlags.Remove(staging.RenamedFlags.First(y => y.Name == x.Name))); // These can be reworked to use asp-for and then we can work off the original objects.

                addedAndDeleted.ForEach(x => staging.Flags.Add(x.Name));
                renamed.ForEach(x => staging.Flags.Add(x.Name));

                staging.Flags = staging.Flags.Distinct().OrderBy(x => x).ToList();

                Message = $"Successfully unstaged flags";
            }
            catch
            {
                Message = "Something went very wrong";
            }

            return Page();
        }

        public async Task<IActionResult> OnPostCommitAsync(string password)
        {
            if (password != staging.Password)
            {
                Message = "Wrong Password";
                return Page();
            }
            try
            {
                // TODO: This needs to be rewritten / burnt / both
                if (staging.DeletedFlags.Any())
                {
                    await Database.DeleteFlagsAsync(staging.DeletedFlags);

                    staging.DeletedFlags
                        .ForEach(flag => System.IO.File.Copy(Path.Combine(FlagsPath, flag.Name + ".png"), Path.Combine(FlagsPath, "dead/", flag.Name + ".png")));
                }

                if (staging.AddedFlags.Any())
                {
                    await Database.InsertFlagsAsync(staging.AddedFlags);

                    staging.AddedFlags
                        .ForEach(flag => System.IO.File.Copy(Path.Combine(FlagsPath, "staging/", flag.Name + ".png"), Path.Combine(FlagsPath, flag.Name + ".png")));
                }

                if (staging.RenamedFlags.Any())
                {
                    await Database.RenameFlagsAsync(staging.RenamedFlags);

                    staging.RenamedFlags
                        .ForEach(flag => System.IO.File.Copy(Path.Combine(FlagsPath, flag.Name + ".png"), Path.Combine(FlagsPath, flag.NewName + ".png")));
                }

                await Database.UpdateKnownFlags();
                staging.Flags = await Database.GetFlags();
                staging.Clear();

                Message = "Changes Commited successfully";
            }
            catch (Exception e)
            {
                Message = "Something went bang\n" + e.Message;
            }

            return Page();
        }

        public IActionResult OnPostDelete(string flag)
        {
            staging.DeletedFlags.Add(new FormFlag
            {
                Name = flag
            });

            staging.Flags.Remove(flag);

            return Page();
        }

        public IActionResult OnPostRename(string flag, string newName)
        {
            if (!(FileNameIsValid(newName)))
            {
                Message = "Invalid Filename.";

                return Page();
            }

            staging.RenamedFlags.Add(new RenameFlag
            {
                Name = flag,
                NewName = newName
            });

            staging.Flags.Remove(flag);

            return Page();
        }

        public async Task<IActionResult> OnPostAddAsync()
        {
            try
            {
                if (!(await ValidateImageAsync()))
                {
                    Message = "Invalid Image.";
                    return Page();
                }

                // TODO: maybe there's something no releasing memory here. - can't Directory.Move().

                using var memoryStream = new MemoryStream();
                await Upload.CopyToAsync(memoryStream);

                memoryStream.Position = 0;

                // Magic.NET is a huge dependency to be used like this
                // Maybe we should switch to a Process and expect to have
                // ImageMagick installed on the target machine.
                using var image = new MagickImage(memoryStream);

                if (ShouldGloss)
                {
                    using var gloss = new MagickImage(Env.WebRootPath + "/gloss.png");

                    gloss.Composite(image, new PointD(0, 0), CompositeOperator.Over);
                }

                image.Write(FlagsPath + "staging/" + Upload.FileName);

                staging.AddedFlags.Add(new FormFlag
                {
                    Name = Path.GetFileNameWithoutExtension(Upload.FileName)
                });

                Message = "Flag uploaded successfully!";

                return Page();
            }
            catch (Exception e)
            {
                Message = $"Something went bang.\n\n\n{e.Message}";

                return Page();
            }
        }

        // TODO: hash images and check against for duplicates.
        // or is that going too far?
        /// <summary>
        /// Rigorously validates an image to ensure it's a flag.
        /// </summary>
        public async Task<bool> ValidateImageAsync()
        {
            var fileName = Path.GetFileNameWithoutExtension(Upload.FileName);

            if (!(FileNameIsValid(fileName))
                || Upload.Length > 15 * 1024 // 15KB
                || Upload.ContentType.ToLower() != "image/png")
            {
                return false;
            }

            using (var memoryStream = new MemoryStream())
            {
                await Upload.CopyToAsync(memoryStream);

                memoryStream.Position = 0;
                using (var image = new MagickImage(memoryStream))
                {
                    if (image.Width != 16 || image.Height != 11)
                    {
                        return false;
                    }
                }

                using (var reader = new BinaryReader(memoryStream))
                {
                    reader.BaseStream.Position = 0;

                    return reader.ReadBytes(PNGHeader.Length).SequenceEqual(PNGHeader);
                }
            }
        }

        /// <summary>
        /// Matches bad things we don't want in filenames.
        /// Inverts the result - returns false on a match.
        /// </summary>
        /// <param name="fileName">The name of the file to validate.</param>
        /// <returns></returns>
        private bool FileNameIsValid(string fileName) =>
            !(fileName == null
                || fileName.Contains("||")
                || fileName.Contains(",")
                || Database.KnownFlags.Contains(fileName)
                || staging.AddedFlags.Select(x => x.Name).Contains(fileName)
                || staging.DeletedFlags.Select(x => x.Name).Contains(fileName)
                || staging.RenamedFlags.Select(x => x.Name).Contains(fileName)
                || staging.RenamedFlags.Select(x => x.NewName).Contains(fileName)
                || fileName.Length > 100);
    }
}