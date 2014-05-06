using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using RoBOSSCommunicator;
using log4net;
using System.IO;
using ProtoBuf;
using roboss;

namespace RobossInterface {

    class RobossDriver {
        private static readonly string ROBOT_TYPE = "RobotCar";
        private static readonly int BUFFER_SIZE = 128;

        private static readonly ILog log = LogManager.GetLogger(typeof(RobossDriver));

        private string hostname;
        private string port;
        private string robotName = null;

        private Robot robot = null;
        
        private ErlComm erlComm;

        private Communicator communicator;

        public RobossDriver(string hostname, string port, string robotName) {
            this.hostname = hostname;
            this.port = port;
            this.robotName = robotName;

            erlComm = new ErlComm();
        }

        public void Connect() {
            communicator = new Communicator();

            string clientName = String.Format("{0}_client",
                robotName == null ? "control" : robotName); 

            if (communicator.Connect(hostname, port, clientName) < 0) {
                log.Fatal("Cannot connect to RoBOSS Controller. Exiting.");
                communicator.Dispose();
                Environment.Exit(1);
            }
            log.Info("Connected to RoBOSS Controller.");

            SendAck();
        }

        public void RequestRobot() {
            if (robotName == null) {
                log.Error("Cannot request robot, because robotName is not set");
                return;
            }

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

            if (robot.Request() < 0) {
                log.Fatal(String.Format("Cannot request robot: {0}. Exiting.", robotName));
                communicator.Dispose();
                Environment.Exit(1);
            }
            log.Info(String.Format("Robot requested: {0}.", robotName));
            
        }

        public void StartLoop() {
            if (communicator == null) {
                log.Error("Cannot start loop before connecting to Controller");
                return;
            }

            var buffer = new byte[BUFFER_SIZE];

            while (true) {
                var readBytes = erlComm.ReadCmd(buffer);

                if (log.IsDebugEnabled) {
                    log.Debug("readBytes = " + readBytes);
                }

                if (readBytes < 0) {
                    log.Info("Broken pipe, exiting.");
                    Environment.Exit(0);
                }

                try {
                    using (var memoryStream = new MemoryStream(buffer)) {
                        var robossRequest = Serializer.Deserialize<RobossRequest>(memoryStream);

                        switch (robossRequest.type) {
                            case RobossRequest.Type.WHEELS_CMD:
                                var wheelsCmd = robossRequest.wheelsCmd;
                                HandleWheelsCmd(wheelsCmd);
                                break;

                            case RobossRequest.Type.STATE_REQUEST:
                                HandleStateRequestCmd();
                                break;

                            case RobossRequest.Type.START:
                                HandleStartCmd();
                                break;

                            case RobossRequest.Type.STOP:
                                HandleStopCmd();
                                break;

                            case RobossRequest.Type.RESET:
                                HandleResetCmd();
                                break;

                            case RobossRequest.Type.ROBOTS_LIST_REQUEST:
                                HandleRobotsListRequest();
                                break;
                        }
                    }
                }
                catch (ProtoException e) {
                    log.Error("Protobuf exception occured: " + e);
                    continue;
                }
                catch (Exception e) {
                    log.Error("Exception occured: " + e);
                    continue;
                }
            }
        }

        private void SendAck() {
            if (log.IsDebugEnabled) {
                log.Debug("Sending ACK message");
            }

            SendMessage(new Ack());
        }

        private void HandleWheelsCmd(WheelsCommand wheelsCmd) {
            if (log.IsDebugEnabled) {
                log.Debug(String.Format("Got WheelsCommand, fl: {0}, fr: {1}, rl: {2}, rr: {3}", 
                    wheelsCmd.frontLeft, wheelsCmd.frontRight, wheelsCmd.rearLeft, wheelsCmd.rearRight));
            }

            if (robot == null) {
                log.Error("Got StateRequest command, but driver was not initialized with robot name");
                return;
            }

            robot.joints[0].motorDesiredVelocity = wheelsCmd.frontLeft;
            robot.joints[1].motorDesiredVelocity = wheelsCmd.frontRight;
            robot.joints[2].motorDesiredVelocity = wheelsCmd.rearLeft;
            robot.joints[3].motorDesiredVelocity = wheelsCmd.rearRight;

            if (robot.Send() < 0) {
                log.Error("Error in sending command to robot");
                return;
            }
        }   

        private void HandleStateRequestCmd() {
            if (log.IsDebugEnabled) {
                log.Debug("Got StateRequest command");
            }

            if (robot == null) {
                log.Error("Got StateRequest command, but driver was not initialized with robot name");
                return;
            }

            if (communicator.Receive(Communicator.RECEIVEBLOCKLEVEL_None) < 0) {
                log.Error("Error occured while receiving simulation state");
                return;
            };

            var robotState = new RobotState();

            unsafe {
                robotState.x = robot.position[0];
                robotState.y = robot.position[1];
                robotState.theta = CalcAngle(robot.rotation[0], robot.rotation[3]);
                robotState.timestamp = communicator.simulationTime;
            }

            if (log.IsDebugEnabled) {
                log.Debug(String.Format("Sending RobotState, x: {0}, y: {1}, theta: {2}, timestamp: {3}",
                    robotState.x, robotState.y, robotState.theta, robotState.timestamp));
            }

            SendMessage(robotState);
        }

        private void HandleRobotsListRequest() {
            if (log.IsDebugEnabled) {
                log.Debug("Got RobotsListsRequest command");
            }

            if (communicator.Receive(Communicator.RECEIVEBLOCKLEVEL_WaitForTimestamp) < 0) {
                log.Error("Error occured while receiving simulation state");
                return;
            };

            var robotsList = new RobotsList();
            for (int i = 0; i < communicator.robotsCount; i++) {
                robotsList.robotNames.Add(communicator.robots[i].name);
            }

            if (log.IsDebugEnabled) {
                log.Debug(String.Format("Sending RobotsList: {0}", string.Join(",", robotsList.robotNames)));
            }

            SendMessage(robotsList);
        }

        private void HandleStartCmd() {
            log.Info("Got Start command");
            communicator.StartSimulation();
        }

        private void HandleStopCmd() {
            log.Info("Got Stop command");
            communicator.StopSimulation();
        }

        private void HandleResetCmd() {
            log.Info("Got Reset command");
            communicator.StopSimulation();
            communicator.ResetSimulation();
        }

        private void SendMessage<T>(T message) {
            try {
                using (var memoryStream = new MemoryStream()) {
                    Serializer.Serialize(memoryStream, message);
                    erlComm.WriteCmd(memoryStream.ToArray(), (int)memoryStream.Position);
                }
            }
            catch (ProtoException e) {
                log.Error("Protobuf exception occured: " + e);
                return;
            }
        }

        private static double CalcAngle(double w, double z) {
            var result = SafeAcos(w) * 2.0;

            // if quaternion z component is positive, the result (in [-pi, pi]) is multiplied by -1
            if (result > Math.PI) {
                result -= 2 * Math.PI;
            }

            if (z > 0) {
                result *= -1;
            }

            return result;
        }

        private static double SafeAcos(double x) {
            if (x <= -1.0f) return Math.PI;//PI;
            if (x >= 1.0f) return 0.0f;
            return Math.Acos(x);
        }

    }
}
