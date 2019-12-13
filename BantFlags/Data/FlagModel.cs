using System.Collections.Generic;

namespace BantFlags.Data
{
    public class FlagModel
    {
        public int PostNumber { get; set; }

        public string Board { get; set; }

        public IEnumerable<string> Flags { get; set; }
    }
}