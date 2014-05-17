#!/usr/bin/env python
__author__ = 'michal'

import sys
import logging
import logging.config
import client_pb2
import traceback
import cPickle
import numpy
import random
import atexit
from robot_model import State
from algorithms.simple import SimpleAlgorithm
from erl_port import ErlangPort
from robot_vis.client import RobotVisClient

class Controller:

    MIN_X = -5.0
    MIN_Y = -5.0

    WORLD_WIDTH = 10.0
    WORLD_HEIGHT = 10.0

    WALL_OFFSET = 1.0

    def __init__(self, robot_name, algorithm):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Controller started")

        self._port = ErlangPort(self._logger)
        self._algorithm = algorithm(self, robot_name)
        self._robot_name = robot_name
        self._target = None

        if self._logger.isEnabledFor(logging.DEBUG):
            self._samples_file = open("/tmp/%s.samples" % (self._robot_name, ), 'w')
        else:
            self._samples_file = None

        self._vis_client = RobotVisClient("127.0.0.1", 9010)

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

        if state_msg.HasField('event'):
            if state_msg.event == client_pb2.StateMessage.STOP:
                self._algorithm.stop()
            elif state_msg.event == client_pb2.StateMessage.START:
                self._algorithm.start()
            elif state_msg.event == client_pb2.StateMessage.RESET:
                self._algorithm.reset()

        for rs in state_msg.robotState:
            state = State.from_full_state(rs)
            states_dict[state.get_robot_name()] = state

        if self._samples_file is not None:
            self._save_sample(('states', states_dict), )

        return states_dict

    def send_vis_update(self, vis_state):
        self._vis_client.send_update(vis_state)

    def get_target(self):
        return self._target

    def obtain_new_target(self):
        target_x = random.uniform(self.MIN_X + self.WALL_OFFSET,
                                  self.MIN_X + self.WORLD_WIDTH - self.WALL_OFFSET)

        target_y = random.uniform(self.MIN_Y + self.WALL_OFFSET,
                                  self.MIN_Y + self.WORLD_HEIGHT - self.WALL_OFFSET)

        self._target = (target_x, target_y)
        self._logger.info("New target obtained: %s",
                          (self._target, ))

        if self._samples_file is not None:
            self._save_sample(('target', self._target), )


    def _save_sample(self, sample):
        if self._samples_file is not None:
            cPickle.dump(sample, self._samples_file)

    def exit(self):
        if self._samples_file is not None:
            self._samples_file.close()

def log_uncaught_exceptions(ex_cls, ex, tb):
    logging.critical('{0}: {1}'.format(ex_cls, ex) + "\n" + "".join(traceback.format_tb(tb)))

def log_numpy_errors(type, flag):
    logging.error("numpy error: {0}, flag: {1}" % (type, flag))

if __name__ == "__main__":
    sys.excepthook = log_uncaught_exceptions

    logging.config.fileConfig('log.config')
    logger = logging.getLogger("main")

    numpy.seterrcall=log_numpy_errors
    numpy.seterr(all='call')

    if len(sys.argv) != 2:
        logger.fatal("Wrong number of arguments. Exiting.")
        sys.exit(0)

    robot_name = sys.argv[1]
    controller = Controller(robot_name, SimpleAlgorithm)

    @atexit.register
    def exit_handler():
        controller.exit()

    #import cProfile
    #cProfile.run('controller.loop()', '/tmp/profile.out')
    controller.loop()



