using System.Data;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data.Database
{
    /// <summary>
    /// Functions for interacting with the database.
    /// </summary>
    public partial class DatabaseService
    {
        private MySqlConnectionPool ConnectionPool { get; }

        public DatabaseService(DatabaseServiceConfig dbConfig)
        {
            ConnectionPool = new MySqlConnectionPool(dbConfig.ConnectionString, dbConfig.PoolSize);
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

        /// <summary>
        /// Returns all of the flags that we support.
        /// </summary>
        public async Task<EnumerableRowCollection<string>> GetFlags()
        {
            using var rentedConnected = await ConnectionPool.RentConnectionAsync();

            DataTable table = await rentedConnected.Object.CreateQuery("SELECT flags.flag FROM flags")
                .ExecuteTableAsync();

            return table.AsEnumerable()
                .Select(x => x.GetValue<string>("flag"));
        }
    }

    /// <summary>
    /// Configuration data passed by appsettings.
    /// </summary>
    public class DatabaseServiceConfig
    {
        public string ConnectionString { get; set; }

        public int PoolSize { get; set; }
    }
}