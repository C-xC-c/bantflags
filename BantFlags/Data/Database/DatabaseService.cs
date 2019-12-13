using System.Collections.Generic;
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

        public string FlagList { get; private set; }

        public HashSet<string> KnownFlags { get; private set; }

        public DatabaseService(DatabaseServiceConfig dbConfig)
        {
            ConnectionPool = new MySqlConnectionPool(dbConfig.ConnectionString, dbConfig.PoolSize);

            UpdateKnownFlags().Wait(); // It's okay to deadlock here since it's only initialised at startup.
        }

        public async Task UpdateKnownFlags()
        {
            var flags = await GetFlags();
            flags.Remove("empty, or there were errors. Re-set your flags.");

            FlagList = string.Join("\n", flags);
            KnownFlags = flags.ToHashSet();
        }

        public async Task DeleteFlagsAsync(List<FormFlag> flags)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();
            using var query = rentedConnection.Object.UseStoredProcedure("delete_flag");

            flags.ForEach(async f =>
                await query.SetParam("@flag", f.Name)
                    .ExecuteNonQueryAsync(reuse: true));
        }

        public async Task RenameFlagsAsync(List<RenameFlag> flags)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();
            using var query = rentedConnection.Object.UseStoredProcedure("rename_flag");

            flags.ForEach(async flag =>
                await query.SetParam("@old", flag.Name)
                    .SetParam("@new", flag.NewName)
                    .ExecuteNonQueryAsync(reuse: true));
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
        }

        /// <summary>
        /// Returns all of the flags that we support.
        /// </summary>
        public async Task<List<string>> GetFlags()
        {
            using var rentedConnected = await ConnectionPool.RentConnectionAsync();

            DataTable table = await rentedConnected.Object.CreateQuery("SELECT flags.flag FROM flags")
                .ExecuteTableAsync();

            return table.AsEnumerable()
                .Select(x => x.GetValue<string>("flag"))
                .ToList();
        }

        public async Task InsertFlagsAsync(List<FormFlag> flags)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();
            using var query = rentedConnection.Object.UseStoredProcedure("insert_flag");

            flags.ForEach(async f =>
                await query.SetParam("@flag", f.Name)
                .ExecuteNonQueryAsync(reuse: true));
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