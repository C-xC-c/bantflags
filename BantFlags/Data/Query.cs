// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using MySql.Data.MySqlClient;
using System;
using System.Data;
using System.Threading.Tasks;

namespace BantFlags.Data
{
    /// <summary>
    /// Succinct methods for creating and executing database queries
    /// </summary>
    public class Query : IDisposable
    {
        private MySqlCommand Command { get; }

        public Query(MySqlCommand cmd)
        {
            Command = cmd;
        }

        public async Task<DataTable> ExecuteTableAsync()
        {
            using (var reader = await Command.ExecuteReaderAsync())
            {
                DataTable table = new DataTable();
                table.Load(reader);

                return table;
            }
        }

        public async Task ExecuteNonQueryAsync(bool reuse = false)
        {
            await Command.ExecuteNonQueryAsync();

            if (!reuse)
            {
                Dispose();
            }
        }

        public Query SetParam(string parameter, object value)
        {
            // When we reuse a query, we write over the parameter.
            if (Command.Parameters.Contains(parameter))
            {
                Command.Parameters[parameter].Value = value;
            }
            else
            {
                Command.Parameters.AddWithValue(parameter, value);
            }

            return this;
        }

        public void Dispose()
        {
            Command.Dispose();
        }
    }
}