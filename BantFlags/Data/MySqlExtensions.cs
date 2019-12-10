using MySql.Data.MySqlClient;
using System;
using System.Data;

namespace BantFlags.Data
{
    public static class MySqlExtensions
    {
        public static Query CreateQuery(this MySqlConnection connection, string sql)
        {
            connection.EnsureConnectionIsOpen();

            return new Query(new MySqlCommand(sql, connection));
        }

        public static Query UseStoredProcedure(this MySqlConnection connection, string sql)
        {
            connection.EnsureConnectionIsOpen();

            return new Query(new MySqlCommand(sql, connection)
            {
                CommandType = CommandType.StoredProcedure
            });
        }

        public static T GetValue<T>(this DataRow row, string column)
        {
            object value = row[column];

            if (value == null || value == DBNull.Value)
                return default;

            return (T)value;
        }

        private static void EnsureConnectionIsOpen(this MySqlConnection connection)
        {
            if (connection.State != ConnectionState.Open)
            {
                connection.Open();
            }
        }
    }
}