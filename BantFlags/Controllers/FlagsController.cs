using BantFlags.Data;
using BantFlags.Data.Database;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
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
            string splitFlag = (version ?? 0) > 1 ? "," : "||"; // comma for v2+, else || for backwards compatibility.

            Result<PostModel> post = PostModel.Create(post_nr, board, regions, splitFlag, Database.KnownFlags);

            if (post.Failed)
            {
                return Problem(post.ErrorMessage, statusCode: StatusCodes.Status400BadRequest);
            }

            await Database.InsertPost(post.Value);

            return Ok(post);
        }

        /// <summary>
        /// Gets the list of supported flags.
        /// </summary>
        [HttpGet]
        [Route("flags")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        public IActionResult Flags() => Ok(Database.FlagList);
    }
}