using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace BantFlags.Data
{
    public class FlagModel
    {
        public int PostNumber { get; set; }

        public string Board { get; set; }

        public IEnumerable<string> Flags { get; set; }
    }
}