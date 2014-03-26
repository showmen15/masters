using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RoBOSSCommunicator;
using log4net;

namespace RobossInterface {

    class RobotDriver {
        private static readonly string ROBOT_TYPE = "RobotCar";
        
        private static readonly ILog log = LogManager.GetLogger(typeof(RobotDriver));

        private string hostname;
        private string port;
        private string robotName;

        private Robot robot;

        private Communicator communicator;

        public RobotDriver(string hostname, string port, string robotName) {
            this.hostname = hostname;
            this.port = port;
            this.robotName = robotName;

            ConnectAndRequestRobot();
        }

        private void ConnectAndRequestRobot() {
            communicator = new Communicator();
            if (communicator.Connect(hostname, port, robotName + "_client") < 0) {
                log.Fatal("Cannot connect to RoBOSS Controller. Exiting.");
                communicator.Dispose();
                Environment.Exit(1);
            }
            log.Info("Connected to RoBOSS Controller.");

            robot = communicator.GetRobotByName(robotName);
            if (robot == null) {
                log.Fatal(String.Format("Cannot find robot: {0}. Exiting.", robotName));
                communicator.Dispose();
                Environment.Exit(1);
            }
            log.Info(String.Format("Robot found: {0}.", robotName));

            if (!ROBOT_TYPE.Equals(robot.type)) {
                log.Fatal(String.Format("Wrong robot type, expected: {0}, got: {1}. Exiting.", 
                    ROBOT_TYPE, robot.type));
                communicator.Dispose();
                Environment.Exit(1);
            }

            if (communicator.RequestRobot(robot.id) < 0) {
                log.Fatal(String.Format("Cannot request robot: {0}. Exiting.", robotName));
                communicator.Dispose();
                Environment.Exit(1);
            }
            log.Info(String.Format("Robot requested: {0}.", robotName));
        }


    }
}
