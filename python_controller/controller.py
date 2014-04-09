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

    def __init__(self, robot_name, algorithm):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Controller started")

        self._port = ErlangPort(self._logger)
        self._algorithm = algorithm(self, robot_name)

        self.send_ack()

    def send_ack(self):
        self._logger.info("Sending Ack message")
        ack_msg = client_pb2.Ack()
        self._port.send_msg(ack_msg.SerializeToString())

    def loop(self):
        self._algorithm.loop()

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

    def request_states(self):
        self.send_request_state()
        state_msg = self.receive_state_msg()

        states_dict = {}

        for rs in state_msg.robotState:
            state = State(rs)
            states_dict[state.get_robot_name()] = state

        return states_dict

def log_uncaught_exceptions(ex_cls, ex, tb):
    logging.critical(''.join(traceback.format_tb(tb)))
    logging.critical('{0}: {1}'.format(ex_cls, ex))

if __name__ == "__main__":
    sys.excepthook = log_uncaught_exceptions

    logging.config.fileConfig('log.config')
    logger = logging.getLogger("main")

    if len(sys.argv) != 2:
        logger.fatal("Wrong number of arguments. Exiting.")
        sys.exit(0)

    robot_name = sys.argv[1]

    controller = Controller(robot_name, SimpleAlgorithm)
    controller.loop()



