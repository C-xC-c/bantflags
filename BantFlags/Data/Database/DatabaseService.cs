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

        private string Flags { get; set; }

        private HashSet<string> FlagsHash { get; set; }

        public DatabaseService(DatabaseServiceConfig dbConfig)
        {
            ConnectionPool = new MySqlConnectionPool(dbConfig.ConnectionString, dbConfig.PoolSize);

            var flags = GetFlags().Result; // It's okay to error here since it's only initialised at startup.

            Flags = string.Join("\n", flags);
            FlagsHash = flags.ToHashSet();
        }

        public string FlagList() => Flags;

        public HashSet<string> KnownFlags() => FlagsHash;

        public async Task UpdateKnownFlags()
        {
            var flags = await GetFlags();

            Flags = string.Join("\n", flags);
            FlagsHash = flags.ToHashSet();
        }

        public async Task DeleteFlagsAsync(List<FormFlag> flags)
        {
            using var rentedConnection = await ConnectionPool.RentConnectionAsync();
            using var query = rentedConnection.Object.UseStoredProcedure("delete_flag");

            flags.ForEach(async f =>
                await query.SetParam("@flag", f.Name)
                    .ExecuteNonQueryAsync(reuse: true));

            return;
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

            return;
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

            return;
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