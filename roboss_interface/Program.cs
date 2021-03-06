﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using log4net;
using log4net.Config;

namespace RobossInterface {
    class Program {
        private static readonly string LOG4NET_CONFIGFILE = "log4net.config";
        private static readonly string ROBOTNAME_VAR = "ROBOTNAME";
        private static readonly string CONTROL_NAME = "control";

        private static readonly ILog log = LogManager.GetLogger(typeof(Program));

        static void Main(string[] args) {
            if (args.Length != 3 && args.Length != 2) {
                System.Environment.SetEnvironmentVariable(ROBOTNAME_VAR, "unknown");
                XmlConfigurator.Configure(new System.IO.FileInfo(LOG4NET_CONFIGFILE));

                log.Fatal(String.Format("Invalid arguments passed: [{0}]", 
                    string.Join(", ", args.Select(v => v.ToString()))));

                Environment.Exit(1);
            }

            string hostname = args[0];
            string port = args[1];
            string robotName;

            if (args.Length == 3) {
                robotName = args[2];
                System.Environment.SetEnvironmentVariable(ROBOTNAME_VAR, robotName);
            } else {
                robotName = null;
                System.Environment.SetEnvironmentVariable(ROBOTNAME_VAR, CONTROL_NAME);
            }
            
            XmlConfigurator.Configure(new System.IO.FileInfo(LOG4NET_CONFIGFILE));

            log.Info(String.Format("Starting with hostname = {0}, port = {1}, robotName = {2}", 
                hostname, port, robotName));

            RobossDriver robossDriver = new RobossDriver(hostname, port, robotName);
            robossDriver.Connect();

            if (robotName != null) {
                robossDriver.RequestRobot();
            }

            robossDriver.StartLoop();
        }
    }
}
