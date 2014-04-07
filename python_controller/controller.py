#!/usr/bin/env python
from webob.client import send_request_app

__author__ = 'michal'

import sys
import logging
import logging.config
import client_pb2
import traceback
from state import State
from algorithms.simple import SimpleAlgorithm
from erl_port import ErlangPort


class Controller:

    def __init__(self, algorithm):
        self._logger = logging.getLogger('controller')
        self._logger.info("Controller started")

        self._port = ErlangPort()
        self._algorithm = algorithm

        self.send_ack()
        self.receive_setup_msg()

    def send_ack(self):
        self._logger.info("Sending Ack message.")
        ack_msg = client_pb2.Ack()
        self._port.send_msg(ack_msg.SerializeToString())

    def receive_setup_msg(self):
        msg = self._port.read_msg()

        try:
            setup_msg = client_pb2.SetupMessage()
            setup_msg.ParseFromString(msg)
            self._expect_setup = False
            self.handle_setup_msg(setup_msg)
        except Exception:
            self._logger.fatal("Failed to parse setup message. Exiting.")
            sys.exit(1)

    def handle_setup_msg(self, setup_msg):
        if self._logger.isEnabledFor('DEBUG'):
            self._logger.debug("Got SetupMessage, robotName = %s" % (setup_msg.robotName, ))

        self._algorithm.set_robot_name(setup_msg.robotName)

    def loop(self):
        while True:
            self.send_request_state()

            state_msg = self.receive_state_msg()
            states = [State(x) for x in state_msg.robotState]

            robot_command = self._algorithm.react(states)

            if robot_command is not None:
                self.send_robot_command(robot_command)


    def send_robot_command(self, robot_command):
        cmd_msg = client_pb2.CommandMessage()
        cmd_msg.type = client_pb2.CommandMessage.ROBOT_COMMAND
        rc = cmd_msg.robotCommand
        rc.frontLeft = robot_command.get_front_left()
        rc.frontRight = robot_command.get_front_right()
        rc.rearLeft = robot_command.get_rear_left()
        rc.rearRight = robot_command.get_rear_right()

        if self._logger.isEnabledFor('DEBUG'):
            self._logger.debug("Sending RobotCommand, fl=%d, fr=%d, rl=%f, rr=%d"
                               % (rc.frontLeft, rc.frontRight, rc.rearLeft, rc.rearRight))

        self._port.send_msg (cmd_msg.SerializeToString())

    def send_request_state(self):

        if self._logger.isEnabledFor('DEBUG'):
            self._logger.debug("Sending RequestState")

        cmd_msg = client_pb2.CommandMessage()
        cmd_msg.type = client_pb2.CommandMessage.REQUEST_STATE
        self._port.send_msg(cmd_msg.SerializeToString())

    def receive_state_msg(self):
        msg = self._port.read_msg()

        try:
            state_msg = client_pb2.StateMessage()
            state_msg.ParseFromString(msg)

            if self._logger.isEnabledFor('DEBUG'):
                self._logger.debug("Received state message, ")

            return state_msg
        except Exception:
            self._logger.fatal("Failed to parse state message. Exiting.")
            sys.exit(1)

def log_uncaught_exceptions(ex_cls, ex, tb):
    logging.critical(''.join(traceback.format_tb(tb)))
    logging.critical('{0}: {1}'.format(ex_cls, ex))

sys.excepthook = log_uncaught_exceptions


def main():
    controller = Controller(SimpleAlgorithm())
    controller.loop()

if __name__ == "__main__":

    logging.config.fileConfig('log.config')

    logger = logging.getLogger("main")

    #def log_uncaught_exceptions(exception_type, exception, tb):
    #    logger.fatal(exception)
    #    sys.excepthook = log_uncaught_exceptions


    main()


