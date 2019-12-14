using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data
{
    public struct Result<T>
    {
        public bool Failed { get; private set; }

        public string ErrorMessage { get; private set; }

        private T _Value { get; set; }

        public T Value => Failed ? throw new Exception() : _Value;

        public Result(bool failed, string error, T value)
        {
            Failed = failed;
            ErrorMessage = error;
            _Value = value;
        }

        public static Result<T> Pass(T value) => new Result<T>(false, default, value);

        public static Result<T> Fail(string error) => new Result<T>(true, error, default);
    }
}