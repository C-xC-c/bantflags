// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using System.Collections.Generic;
using System.Linq;

namespace BantFlags.Data
{
    public class PostModel
    {
        public int PostNumber { get; private set; }

        public string Board { get; private set; }

        public string[] Flags { get; private set; }

        private PostModel(int post_nr, string board, string[] flags)
        {
            PostNumber = post_nr;
            Board = board;
            Flags = flags;
        }

        public static (PostModel, string) Create(string post_nr, string board, string regions, string splitFlag, HashSet<string> knownFlags, HashSet<string> boards)
        {
            string[] empty = { "empty, or there were errors. Re-set your flags." };

            if (!int.TryParse(post_nr, out int postNumber))
                return (default, "Invalid post number.");

            if (!boards.Contains(board))
                return (default, "Invalid board parameter.");

            if (regions == null)
                return (new PostModel(postNumber, board, empty), default);

            var flags = regions.Split(splitFlag);

            if (flags.Count() > 30)
                return (default, "Too many flags.");

            foreach (string flag in flags)
            {
                if (!knownFlags.Contains(flag)) // Not ideal but it's better than doing it in the controller or passing the database here.
                    return (new PostModel(postNumber, board, empty), default);
            }

            return (new PostModel(postNumber, board, flags), default);
        }
    }
}