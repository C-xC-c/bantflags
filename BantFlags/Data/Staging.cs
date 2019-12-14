using System.Collections.Generic;

namespace BantFlags.Data
{
    /// <summary>
    /// Singleton class comprised of objects used in/Upload.
    /// It's a fucking mess and I hate it so much.
    /// </summary>
    public class Staging
    {
        public List<RenameFlag> RenamedFlags { get; set; }
        public List<FormFlag> DeletedFlags { get; set; }
        public List<FormFlag> AddedFlags { get; set; }

        /// <summary>
        /// The current list of resolved flags including changes currently in Staging.
        /// Exists here since it's a singleton.
        /// </summary>
        public List<string> Flags { get; set; }

        /// <summary>
        /// Used for commiting and unstaging staged flags.
        /// </summary>
        public string Password { get; }

        public Staging(string password)
        {
            RenamedFlags = new List<RenameFlag>();
            DeletedFlags = new List<FormFlag>();
            AddedFlags = new List<FormFlag>();
            Password = password;
        }

        public void Clear()
        {
            RenamedFlags = new List<RenameFlag>();
            DeletedFlags = new List<FormFlag>();
            AddedFlags = new List<FormFlag>();
        }
    }

    public class FormFlag
    {
        public string Name { get; set; }
        public bool IsChecked { get; set; }

        public Method FormMethod { get; set; }
    }

    public class RenameFlag : FormFlag
    {
        public string NewName { get; set; }
    }

    public enum Method
    {
        Delete,

        Rename,

        Add
    }
}