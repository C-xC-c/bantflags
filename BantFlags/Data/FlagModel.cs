using System.Collections.Generic;
using System.Linq;

namespace BantFlags.Data
{
    public class FlagModel
    {
        public int PostNumber { get; private set; }

        public string Board { get; private set; }

        public string[] Flags { get; private set; }

        private FlagModel(int post_nr, string board, string[] flags)
        {
            PostNumber = post_nr;
            Board = board;
            Flags = flags;
        }

        /// <summary>
        /// A wrapper around post validation so it's all in one place.
        /// </summary>
        public static (bool, FlagModel, string) Create(string post_nr, string board, string regions, string splitFlag, HashSet<string> knownFlags)
        {
            if (!int.TryParse(post_nr, out int postNumber))
                return (false, default, "Invalid post number.");

            if (board != "bant")
                return (false, default, "Invalid board parameter.");

            if (regions == null)
                regions = "somebrokenflagstringsothatwegettheemptyflagwhenweshould";

            var flags = regions.Split(splitFlag);

            if (flags.Count() > 30)
                return (false, default, "Too many flags.");

            foreach (string flag in flags)
            {
                if (!knownFlags.Contains(flag)) // Not ideal but it's better than doing it in the controller or passing the database here.
                {
                    flags = new string[] { "empty, or there were errors. Re-set your flags." };
                    break;
                }
            }

            return (true, new FlagModel(postNumber, board, flags), default);
        }
    }
}