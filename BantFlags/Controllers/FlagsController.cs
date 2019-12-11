using BantFlags.Data;
using BantFlags.Data.Database;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
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

        private string FlagList { get; set; }

        private HashSet<string> DatabaseFlags { get; set; }

        public FlagsController(DatabaseService db)
        {
            Database = db;

            // During initialisation we get the current list of flags for resolving supported flags and preventing duplicate flags from being created
            var flags = Database.GetFlags().Result; // If this fails the program should exit anyway.

            FlagList = string.Join("\n", flags);
            DatabaseFlags = flags.ToHashSet();
        }

        [HttpPost]
        [Route("get")]
        [Consumes("application/x-www-form-urlencoded")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public async Task<IActionResult> Get([FromForm]string post_nrs, [FromForm]string board, [FromForm]string version)
        {
            try
            {
                int ver = int.TryParse(version, out int x) ? x : 0;

                if (ver > 1)
                {
                    // Improved flag sending, see Docs/GetPosts
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

        [HttpPost]
        [Route("post")]
        [Consumes("application/x-www-form-urlencoded")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public async Task<IActionResult> Post([FromForm]string post_nr, [FromForm]string board, [FromForm]string regions)
        {
            try // We only care if the post if valid.
            {
                // TODO: Currently we skip over invalid flags. Should we error instead?
                var flags = regions.Split("||").Where(x => DatabaseFlags.Contains(x));

                FlagModel post = new FlagModel
                {
                    PostNumber = int.TryParse(post_nr, out int temp) ? temp : throw new FormatException("Bad post number."),
                    Board = board == "bant" ? "bant" : throw new FormatException("Board parameter wasn't formatted correctly."),
                    Flags = flags.Count() > 0 ? flags : throw new FormatException("Your post didn't include any flags, or your flags were invalid.")
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
        public IActionResult Flags() => Ok(FlagList);

        /// <summary>
        /// Creates an error mesage to send in case of 400 bad request, without giving away too much information.
        /// </summary>
        /// <param name="exception">Raw exception to be filtered.</param>
        private string ErrorMessage(Exception exception) =>
            exception switch
            {
                FormatException e => e.Message,
                DbException _ => "Internal database error.",
                ArgumentNullException _ => "No regions sent",
                Exception e => e.Message, // Don't do this.
                _ => "how in the hell"
            }; // This needs more testing.
    }
}