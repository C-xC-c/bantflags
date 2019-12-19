// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data.Database
{
    public partial class DatabaseService
    {
        // Maybe this could be better but I don't know SQL lol
        private readonly string GetPostsQuery = @"SELECT posts.post_nr, flags.flag FROM flags LEFT JOIN (postflags) ON (postflags.flag = flags.id) LEFT JOIN (posts) ON (postflags.post_nr = posts.id) WHERE FIND_IN_SET(posts.post_nr, (@posts)) AND posts.board = @board";

        /// <summary>
        /// Returns the post numbers and their flags from the post numbers in the input.
        /// </summary>
        /// <param name="post_nr">List of post numbers on the page.</param>
        public async Task<IEnumerable<IGrouping<int, DataRow>>> GetPosts(string post_nr, string board)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();

            DataTable table = await rentedConnection.Object.CreateQuery(GetPostsQuery)
                .SetParam("@posts", post_nr)
                .SetParam("@board", board)
                .ExecuteTableAsync();

            return table.AsEnumerable()
                .GroupBy(x => x.GetValue<int>("post_nr"));
        }

        public async Task<List<Dictionary<string, string>>> GetPosts_V1(string post_nr, string board)
        {
            List<Dictionary<string, string>> posts = new List<Dictionary<string, string>>();

            var x = await GetPosts(post_nr, board);
            x.ForEach(x => posts.Add(new Dictionary<string, string>
                {
                    {"post_nr", x.Key.ToString() },
                    {"region", string.Join("||", x.AsEnumerable().Select(y => y.GetValue<string>("flag")))}
                }));

            return posts;
        }

        public async Task<Dictionary<int, IEnumerable<string>>> GetPosts_V2(string post_nr, string board)
        {
            var posts = await GetPosts(post_nr, board);
            return posts
                .ToDictionary(
                    x => x.Key,
                    x => x.AsEnumerable().Select(x => x.GetValue<string>("flag"))
                );
        }
    }
}