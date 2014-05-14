#!/usr/bin/env python
from webob.client import send_request_app

__author__ = 'michal'

import sys
import logging
import logging.config
import client_pb2
import traceback
import cPickle
import numpy
from robot_model import State
from algorithms.simple import SimpleAlgorithm
from erl_port import ErlangPort
from robot_vis.client import RobotVisClient

class Controller:

    SAVE_SAMPLES = 1000

    def __init__(self, robot_name, algorithm):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Controller started")

        self._port = ErlangPort(self._logger)
        self._algorithm = algorithm(self, robot_name)
        self._robot_name = robot_name
        self._samples = []
        self._samples_counter = self.SAVE_SAMPLES
        self._samples_from_file = False

        self._vis_client = RobotVisClient("127.0.0.1", 9010)



        self.send_ack()

    def send_ack(self):
        self._logger.info("Sending Ack message")
        ack_msg = client_pb2.Ack()
        self._port.send_msg(ack_msg.SerializeToString())

    def loop(self):
        self._algorithm.loop()

    def send_robot_command(self, robot_command):
        if self._samples_from_file:
            return

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
        if self._samples_from_file:
            return

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
        states_dict = None

        if self._samples_from_file:
            if self._samples_counter == len(self._samples):
                sys.exit(0)

            states_dict = self._samples[self._samples_counter]
            self._samples_counter += 1
            if self._samples_counter == 20:
                self._algorithm.reset()

        else:
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

            if self._samples_counter > 0:
                self._save_sample(states_dict)

        return states_dict

    def send_vis_update(self, vis_state):
        self._vis_client.send_update(vis_state)

    def _save_sample(self, sample):
        self._samples.append(sample)

        self._samples_counter -= 1
        if self._samples_counter == 0:
            f = open("/tmp/%s.samples" % (self._robot_name, ), 'w')
            cPickle.dump(self._samples, f)
            f.close()

    def load_samples(self, filename):
        self._samples_from_file = True
        self._samples_counter = 0

        f = open(filename, 'r')
        self._samples = cPickle.load(f)
        f.close()


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

    if len(sys.argv) not in [2, 3]:
        logger.fatal("Wrong number of arguments. Exiting.")
        sys.exit(0)

    robot_name = sys.argv[1]

    controller = Controller(robot_name, SimpleAlgorithm)
    if len(sys.argv) == 3:
        controller.load_samples(sys.argv[2])

    #import cProfile
    #cProfile.run('controller.loop()', '/tmp/profile.out')
    controller.loop()



