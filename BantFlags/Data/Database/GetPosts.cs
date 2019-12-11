using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data.Database
{
    public partial class DatabaseService
    {
        private readonly string GetPostsQuery = @"SELECT posts.post_nr, flags.flag FROM flags LEFT JOIN (postflags) ON (postflags.flag = flags.id) LEFT JOIN (posts) ON (postflags.post_nr = posts.id) WHERE FIND_IN_SET(posts.post_nr, (@posts))";

        /// <summary>
        /// Returns the post numbers and their flags from the post numbers in the input.
        /// </summary>
        /// <param name="input">List of post numbers on the page.</param>
        /// <returns></returns>
        public async Task<IEnumerable<IGrouping<int, DataRow>>> GetPosts(string input)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();

            DataTable table = await rentedConnection.Object.CreateQuery(GetPostsQuery)
                .SetParam("@posts", input)
                .ExecuteTableAsync();

            return table.AsEnumerable()
                .GroupBy(x => x.GetValue<int>("post_nr"));
        }

        public async Task<List<Dictionary<string, string>>> GetPosts_V1(string input)
        {
            List<Dictionary<string, string>> posts = new List<Dictionary<string, string>>();

            var x = await GetPosts(input);

            x.ForEach(x => posts.Add(new Dictionary<string, string>
                {
                    {"post_nr", x.Key.ToString() },
                    {"region", string.Join("||", x.AsEnumerable().Select(y => y.GetValue<string>("flag")))}
                }));

            return posts;
        }

        public async Task<Dictionary<int, IEnumerable<string>>> GetPosts_V2(string input)
        {
            var posts = await GetPosts(input);
            return posts
                .ToDictionary(
                    x => x.Key,
                    x => x.AsEnumerable().Select(x => x.GetValue<string>("flag"))
                );
        }
    }
}