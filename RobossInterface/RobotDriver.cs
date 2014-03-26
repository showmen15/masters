using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RoBOSSCommunicator;
using log4net;

namespace RobossInterface {

    class RobotDriver {
        private static readonly ILog log = LogManager.GetLogger(typeof(RobotDriver));

        private string hostname;
        private string port;
        private string robotName;

        private Communicator communicator;

        public RobotDriver(string hostname, string port, string robotName) {

            this.hostname = hostname;
            this.port = port;
            this.robotName = robotName;

            this.communicator = new Communicator();
            if (communicator.Connect(hostname, port, robotName + "_client") < 0) {
                log.Fatal("Cannot connect to RoBOSS Controller.");
                communicator.Dispose();
                return;
            }

            log.Info("Connected to RoBOSS Controller.");

            communicator.Dispose();

        }
    }
}
