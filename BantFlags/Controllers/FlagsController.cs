using BantFlags.Data;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
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

        private ILogger Logger { get; }

        private string FlagList { get; set; }

        private HashSet<string> DatabaseFlags { get; set; }

        public FlagsController(DatabaseService db, ILogger<FlagsController> logger)
        {
            Database = db;
            Logger = logger;

            // During initialisation we get the current list of flags for
            // resolving supported flags and preventing duplicate flags from
            // being created
            List<string> flags = Database.GetFlags().Result;

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
                var posts = await Database.GetPosts(post_nrs);

                return Json(posts);
            }
            catch (Exception e)
            {
                return Problem(e.Message, statusCode: StatusCodes.Status400BadRequest); // TODO: We shouldn't send the exception message
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
        public IActionResult Flags() => Ok(FlagList);

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