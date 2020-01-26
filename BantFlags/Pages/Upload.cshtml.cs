// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using BantFlags.Data;
using BantFlags.Data.Database;
using ImageMagick;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags
{
    public class UploadModel : PageModel
    {
        private DatabaseService Database { get; set; }
        public Staging StagedFlags { get; set; }
        public string Message { get; private set; }

        private string WebRoot { get; }

        public HashSet<string> AllNames => StagedFlags.Names.Concat(StagedFlags.Flags.Select(x => x.Name)).ToHashSet();

        public UploadModel(DatabaseService dbs, Staging ns, IWebHostEnvironment env)
        {
            Database = dbs;

            StagedFlags = ns;

            WebRoot = env.WebRootPath;
        }

        public void OnGet()
        {
            StagedFlags.Names = StagedFlags.Names ?? Database.KnownFlags;
        }

        public IActionResult OnPostDelete(string flag)
        {
            var stagingFlag = Flag.CreateFromDelete(flag).Value;

            StagedFlags.Flags.Add(stagingFlag);
            StagedFlags.Names.Remove(stagingFlag.Name);

            Message = $"{stagingFlag.Name} deleted.";
            return Page();
        }

        public IActionResult OnPostRename(string flag, string newName)
        {
            var stagingFlag = Flag.CreateFromRename(flag, newName, AllNames);

            if (stagingFlag.Failed)
            {
                Message = stagingFlag.ErrorMessage;
                return Page();
            }

            StagedFlags.Flags.Add(stagingFlag.Value);
            StagedFlags.Names.Add(stagingFlag.Value.Name);

            Message = $"{stagingFlag.Value.OldName} renamed to {stagingFlag.Value.Name}.";
            return Page();
        }

        public async Task<IActionResult> OnPostAddAsync(IFormFile upload, bool gloss)
        {
            var stagingFlag = await Flag.CreateFromFile(upload, AllNames);

            if (stagingFlag.Failed)
            {
                Message = stagingFlag.ErrorMessage;
                return Page();
            }

            using var memoryStream = new MemoryStream();
            await upload.CopyToAsync(memoryStream);

            memoryStream.Position = 0;

            // Magic.NET is a huge dependency to be used like this
            // Maybe we should switch to a Process and expect to have
            // ImageMagick installed on the target machine.
            using var image = new MagickImage(memoryStream);

            if (gloss)
            {
                using var glossImage = new MagickImage(WebRoot + "/gloss.png");

                glossImage.Composite(image, new PointD(0, 0), CompositeOperator.Over);
            }

            image.Write(WebRoot + "/flags/staging/" + upload.FileName);

            StagedFlags.Flags.Add(stagingFlag.Value);

            Message = $"{stagingFlag.Value.Name} uploaded";
            return Page();
        }

        public IActionResult OnPostUnstage(Flag[] flags, string password)
        {
            if (password != StagedFlags.Password)
            {
                Message = "Incorrect Password";
                return Page();
            }

            for (int i = flags.Length - 1; i >= 0; i--)
            {
                if (flags[i].IsChecked != true)
                {
                    continue;
                }

                StagedFlags.Flags.RemoveAt(i);

                var flag = flags[i];
                switch (flag.FlagMethod)
                {
                    case Method.Add:
                        System.IO.File.Delete(WebRoot + "/flags/staging/" + flag.Name);
                        StagedFlags.Names.Remove(flag.Name);
                        break;

                    case Method.Delete:
                        StagedFlags.Names.Add(flag.Name);
                        break;

                    case Method.Rename:
                        StagedFlags.Names.Remove(flag.Name);
                        break;

                    default:
                        throw new Exception();
                }
            }

            Message = "Removed flags from staging";
            return Page();
        }

        public async Task<IActionResult> OnPostCommit(string password)
        {
            if (password != StagedFlags.Password)
            {
                Message = "Incorrect Password";
                return Page();
            }

            foreach (var flag in StagedFlags.Flags)
            {
                string flagname = flag.Name + ".png";

                switch (flag.FlagMethod)
                {
                    case Method.Add:
                        await Database.InsertFlagAsync(flag);
                        Directory.Move(WebRoot + "/flags/staging/" + flagname, WebRoot + "/flags/" + flagname);
                        break;

                    case Method.Delete:
                        await Database.DeleteFlagAsync(flag);
                        Directory.Move(WebRoot + "/flags/" + flagname, WebRoot + "/flags/dead/" + Guid.NewGuid().ToString()); // Use a GUID so flags with the same name can be deleted.
                        break;

                    case Method.Rename:
                        await Database.RenameFlagAsync(flag);
                        Directory.Move(WebRoot + "/flags/" + flag.OldName + ".png", WebRoot + "/flags/" + flagname);
                        break;

                    default:
                        throw new Exception();
                }
            }

            await Database.UpdateKnownFlags();
            StagedFlags.Names = Database.KnownFlags;
            StagedFlags.Clear();

            Message = "Changes committed successfully";
            return Page();
        }
    }
}