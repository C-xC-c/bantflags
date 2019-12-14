using System;
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

        public static Result<PostModel> Create(string post_nr, string board, string regions, string splitFlag, HashSet<string> knownFlags)
        {
            string[] empty = new string[] { "empty, or there were errors. Re-set your flags." };

            if (!int.TryParse(post_nr, out int postNumber))
                return Result<PostModel>.Fail("Invalid post number.");

            if (board != "bant")
                return Result<PostModel>.Fail("Invalid board parameter.");

            if (regions == null)
                return Result<PostModel>.Pass(new PostModel(postNumber, board, empty));

            var flags = regions.Split(splitFlag);

            if (flags.Count() > 30)
                return Result<PostModel>.Fail("Too many flags.");

            foreach (string flag in flags)
            {
                if (!knownFlags.Contains(flag)) // Not ideal but it's better than doing it in the controller or passing the database here.
                    return Result<PostModel>.Pass(new PostModel(postNumber, board, empty));
            }

            return Result<PostModel>.Pass(new PostModel(postNumber, board, flags));
        }
    }
}