// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using System;

namespace BantFlags.Data
{
    public class PoolObject<T> : IDisposable
    {
        public T Object { get; }

        private Action<T> ReturnAction { get; }

        public PoolObject(T o, Action<T> returnAction)
        {
            Object = o;
            ReturnAction = returnAction;
        }

        public void Dispose()
        {
            ReturnAction(Object);
        }

        public static implicit operator T(PoolObject<T> poolObject)
            => poolObject.Object;
    }
}