using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using log4net;
using log4net.Config;

namespace RobossInterface {
    class Program {
        private static readonly string LOG4NET_CONFIGFILE = "log4net.config";
        private static readonly string ROBOTNAME_VAR = "ROBOTNAME";

        private static readonly ILog log = LogManager.GetLogger(typeof(Program));

        static void Main(string[] args) {
            if (args.Length != 3) {
                System.Environment.SetEnvironmentVariable(ROBOTNAME_VAR, "unknown");
                XmlConfigurator.Configure(new System.IO.FileInfo(LOG4NET_CONFIGFILE));

                log.Fatal(String.Format("Invalid arguments passed: [{0}]", 
                    string.Join(", ", args.Select(v => v.ToString()))));

                Environment.Exit(1);
            }

            string hostname = args[0];
            string port = args[1];
            string robotName = args[2];

            System.Environment.SetEnvironmentVariable(ROBOTNAME_VAR, robotName);
            XmlConfigurator.Configure(new System.IO.FileInfo(LOG4NET_CONFIGFILE));

            log.Info(String.Format("Starting with hostname = {0}, port = {1}, robotName = {2}", 
                hostname, port, robotName));

            RobotDriver robotDriver = new RobotDriver(hostname, port, robotName);
        }
    }
}
