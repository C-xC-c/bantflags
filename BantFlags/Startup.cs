// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>
using BantFlags.Data;
using BantFlags.Data.Database;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.DataProtection;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.HttpOverrides;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.FileProviders;
using Microsoft.Extensions.Hosting;
using System.IO;
using System.Runtime.InteropServices;

namespace BantFlags
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddControllers().AddNewtonsoftJson();

            services.AddRazorPages();

            // TODO: this shouldn't just be for Linux.
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) // For image upload during production.
            {
                services.AddDataProtection()
                    .SetApplicationName("BantFlags")
                    .PersistKeysToFileSystem(new DirectoryInfo(Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "keys")));
            }

            services.AddSingleton(new DatabaseService(Configuration.GetSection("dbconfig").Get<DatabaseServiceConfig>()));
            services.AddSingleton(new Staging(Configuration.GetValue<string>("staging-password")));
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            string webroot = Configuration.GetValue<string>("webroot");
            if (Directory.Exists(webroot))
            {
                env.WebRootPath = webroot;
                env.WebRootFileProvider = new PhysicalFileProvider(env.WebRootPath);
            }

            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            app.UseForwardedHeaders(new ForwardedHeadersOptions
            {
                ForwardedHeaders = ForwardedHeaders.XForwardedFor | ForwardedHeaders.XForwardedProto
            });

            app.UseStaticFiles(new StaticFileOptions
            {
                ServeUnknownFileTypes = true,
                DefaultContentType = "text/plain"
            });

            app.UseRouting();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapRazorPages();
                endpoints.MapControllers();
            });
        }
    }
}