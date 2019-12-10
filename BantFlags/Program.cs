using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System;
using System.IO;

namespace BantFlags
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateHostBuilder(args).Build().Run();
        }

        public static IHostBuilder CreateHostBuilder(string[] args) =>
            Host.CreateDefaultBuilder(args)
                .ConfigureWebHostDefaults(webBuilder =>
                {
                    webBuilder.UseStartup<Startup>();
                })
                .ConfigureAppConfiguration((host, config) =>
                {
                    // Explicitly look for appsettings.json in the program's directory
                    config.AddJsonFile(Path.Join(AppDomain.CurrentDomain.BaseDirectory + "appsettings.json"), optional: false, reloadOnChange: false);
                });
    }
}