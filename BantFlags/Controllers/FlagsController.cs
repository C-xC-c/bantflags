using BantFlags.Data;
using BantFlags.Data.Database;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Data.Common;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Controllers
{
    [ApiController]
    [Route("api")]
    public class FlagsController : Controller
    {
        private DatabaseService Database { get; }

        public FlagsController(DatabaseService db)
        {
            Database = db;
        }

        /// <summary>
        /// Retrives flags from the database from the posts sent in post_nrs
        /// </summary>
        /// <param name="post_nrs">The comma seperated list of post numbers from the thread.</param>
        /// <param name="board">Currently should only be /bant/. Not checked here because we don't need to care what they send.</param>
        /// <param name="version">The version of the userscript.</param>
        [HttpPost]
        [Route("get")]
        [Consumes("application/x-www-form-urlencoded")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public async Task<IActionResult> Get([FromForm]string post_nrs, [FromForm]string board, [FromForm]int? version)
        {
            try // We only care if the post if valid.
            {
                int ver = version ?? 0;

                if (ver > 1)
                {
                    // Improved data structuring, see Docs/GetPosts
                    return Json(await Database.GetPosts_V2(post_nrs));
                }
                else
                {
                    return Json(await Database.GetPosts_V1(post_nrs));
                }
            }
            catch (Exception e)
            {
                return Problem(ErrorMessage(e), statusCode: StatusCodes.Status400BadRequest);
            }
        }

        /// <summary>
        /// Posts flags in the database.
        /// </summary>
        /// <param name="post_nr">The post number to associate the flags to.</param>
        /// <param name="board">Currently should only be /bant/.</param>
        /// <param name="regions">List of flags to associate with the post. Split by "||" in API V1 and "," in V2.</param>
        /// <param name="version">The version of the userscript.</param>
        [HttpPost]
        [Route("post")]
        [Consumes("application/x-www-form-urlencoded")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public async Task<IActionResult> Post([FromForm]string post_nr, [FromForm]string board, [FromForm]string regions, [FromForm]int? version)
        {
            try // We only care if the post if valid.
            {
                string[] flags;
                int ver = version ?? 0;

                if (ver > 1)
                {
                    flags = regions.Split(",");
                }
                else
                {
                    flags = regions.Split("||");
                }

                // TODO: Currently we skip over invalid flags. Should we error instead?
                // We can't easily format it like in the current bantflags - we really should continue to
                // return "empty, or there were errors. Re-set your flags.", for compatibility, but we'd
                // have to store that as a flag in the database and perform an expensive string comparison
                // to stop people selecting it.
                // Do we care if people select the broken flag?
                var validFlags = flags.Where(x => Database.KnownFlags().Contains(x));

                for (int i = 0; i < flags.Length; i++)
                {
                    if (!Database.KnownFlags().Contains(flags[i]))
                    {
                        flags[i] = "empty, or there were errors. Re-set your flags.";
                    }
                }

                var numberOfFlags = validFlags.Count();
                if (numberOfFlags <= 0 || numberOfFlags > 25)
                {
                    throw new ArgumentException("Your post didn't include any flags, or your flags were invalid.");
                }

                FlagModel post = new FlagModel
                {
                    PostNumber = int.TryParse(post_nr, out int temp) ? temp : throw new ArgumentException("Invalid post number."),
                    Board = board == "bant" ? "bant" : throw new ArgumentException("Board parameter wasn't formatted correctly."),
                    Flags = validFlags
                };

                await Database.InsertPost(post);

                return Ok(post);
            }
            catch (Exception e)
            {
                return Problem(detail: ErrorMessage(e), statusCode: StatusCodes.Status400BadRequest);
            }
        }

        [HttpGet]
        [Route("flags")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        public IActionResult Flags() => Ok(Database.FlagList());

        /// <summary>
        /// Creates an error mesage to send in case of 400 bad request, without giving away too much information.
        /// </summary>
        /// <param name="exception">Raw exception to be filtered.</param>
        private string ErrorMessage(Exception exception) =>
            exception switch
            {
                NullReferenceException _ => "Some data wasn't initialised. Are you sending everything?",
                DbException _ => "Internal database error.",
                ArgumentNullException _ => "No regions sent",
                ArgumentException e => e.Message, // We create all arguement exceptions here, we can just pass the message on.
                Exception e => e.Message, // Don't do this.
                _ => "how in the hell"
            }; // This needs more testing.
    }
}