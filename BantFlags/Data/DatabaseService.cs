using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data
{
    public class DatabaseService
    {
        private MySqlConnectionPool ConnectionPool { get; }

        public DatabaseService(DatabaseServiceConfig dbConfig)
        {
            ConnectionPool = new MySqlConnectionPool(dbConfig.ConnectionString, dbConfig.PoolSize);
        }

        private readonly string SelectQuery = @"SELECT posts.post_nr, flags.flag FROM flags LEFT JOIN (postflags) ON (postflags.flag = flags.id) LEFT JOIN (posts) ON (postflags.post_nr = posts.id) WHERE FIND_IN_SET(posts.post_nr, (@posts))";

        public async Task<List<Dictionary<string, string>>> GetPosts(string input)
        {
            List<Dictionary<string, string>> posts = new List<Dictionary<string, string>>();

            using (var rentedConnection = await ConnectionPool.RentConnectionAsync())
            {
                DataTable table = await rentedConnection.Object.CreateQuery(SelectQuery)
                    .SetParam("@posts", input)
                    .ExecuteTableAsync();

                // TODO: rework this.
                // Once the majority are on the new script we can do the below
                // and return Dictionary<int, IEnumerable<string>>
                // instead of rewriting the flags each time.
                var groupedPosts = table.AsEnumerable()
                    .GroupBy(x => x.GetValue<int>("post_nr"));

                //.ToDictionary(
                //    x => x.Key,
                //    x => x.AsEnumerable().Select(x => x.GetValue<string>("flag"));

                groupedPosts.ForEach(x => posts.Add(
                new Dictionary<string, string>
                {
                    {"post_nr", x.Key.ToString() },
                    // This is a lot of work, it'll be nice to get rid of it.
                    {"region", string.Join("||", x.AsEnumerable().Select(y => y.GetValue<string>("flag")))}
                }
                ));

                return posts;
            }
        }

        public async Task InsertPost(FlagModel post)
        {
            using (var rentedConnection = await ConnectionPool.RentConnectionAsync())
            {
                await rentedConnection.Object.UseStoredProcedure("insert_post")
                    .SetParam("@post_nr", post.PostNumber)
                    .SetParam("@board", post.Board)
                    .ExecuteNonQueryAsync();

                using (var query = rentedConnection.Object.UseStoredProcedure("insert_post_flags"))
                {
                    query.SetParam("@post_nr", post.PostNumber);

                    post.Flags.ForEach(async f =>
                        await query.SetParam("@flag", f)
                        .ExecuteNonQueryAsync(reuse: true));
                }
            }

            return;
        }

        public async Task<List<string>> GetFlags()
        {
            using (var rentedConnected = await ConnectionPool.RentConnectionAsync())
            {
                DataTable table = await rentedConnected.Object.CreateQuery("SELECT flags.flag FROM flags")
                    .ExecuteTableAsync();

                return table.AsEnumerable()
                    .Select(x => x.GetValue<string>("flag"))
                    .ToList();
            }
        }
    }

    public class DatabaseServiceConfig
    {
        public string ConnectionString { get; set; }

        public int PoolSize { get; set; }
    }
}