// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using MySql.Data.MySqlClient;
using Nito.AsyncEx;
using System;
using System.Data;
using System.Threading.Tasks;

namespace BantFlags.Data
{
    public class MySqlConnectionPool : IDisposable
    {
        public AsyncCollection<MySqlConnection> Connections { get; }

        protected string ConnectionString { get; }

        protected int PoolSize { get; }

        public MySqlConnectionPool(string connectionString, int poolSize)
        {
            PoolSize = poolSize;
            ConnectionString = connectionString;

            Connections = new AsyncCollection<MySqlConnection>(poolSize);

            for (int i = 0; i < poolSize; i++)
            {
                var connection = new MySqlConnection(connectionString);

                connection.Open();

                Connections.Add(connection);
            }
        }

        public async Task<PoolObject<MySqlConnection>> RentConnectionAsync()
        {
            return new PoolObject<MySqlConnection>(await Connections.TakeAsync(), obj =>
            {
                if (obj.State != ConnectionState.Open)
                {
                    obj.Open();
                }

                Connections.Add(obj);
            });
        }

        public void Dispose()
        {
            for (int i = 0; i < PoolSize; i++)
            {
                var connection = Connections.Take();

                connection.Dispose();
            }
        }
    }
}