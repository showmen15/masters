#!/usr/bin/env python
__author__ = 'michal'

import sys
import logging
import logging.config
import traceback
import cPickle
import numpy
import json
from algorithms.simple import SimpleAlgorithm
from algorithms.go_and_turn import GoAndTurn
from algorithms.fearful import FearfulAlgorithm
from robot_vis.client import RobotVisClient
from collections import deque

class MockController:

    MIN_X = -5.0
    MIN_Y = -5.0

    WORLD_WIDTH = 10.0
    WORLD_HEIGHT = 10.0

    WALL_OFFSET = 1.0

    def __init__(self, samples_file, robot_name, algorithm, roson):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Controller started")

        self._algorithm = algorithm(self, robot_name, roson)
        self._robot_name = robot_name

        self._states = None
        self._targets = None
        self._target = None

        self._load_samples(samples_file)

        self._vis_client = RobotVisClient("127.0.0.1", 9010)

    def loop(self):
        self._algorithm.loop()

    def send_robot_command(self, robot_command):
        pass

    def send_request_state(self):
        pass

    def request_states(self):
        try:
            return self._states.popleft()
        except IndexError:
            sys.exit(0)

    def send_vis_update(self, vis_state):
        self._vis_client.send_update(vis_state)

    def get_new_target(self):
        self._obtain_new_target()
        return self._target

    def _obtain_new_target(self):
        try:
            self._target = self._targets.popleft()
        except IndexError:
            sys.exit(0)

    def _load_samples(self, filename):
        self._states = deque()
        self._targets = deque()

        f = open(filename, 'r')

        try:
            while True:
                sample = cPickle.load(f)
                if sample[0] == 'states':
                    self._states.append(sample[1])
                elif sample[0] == 'target':
                    self._targets.append(sample[1])

        except EOFError:
            pass

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

    if len(sys.argv) != 4:
        logger.fatal("Wrong number of arguments. Exiting.")
        sys.exit(0)

    robot_name = sys.argv[1]
    roson_filename = sys.argv[2]
    samples_file = sys.argv[3]

    robot_name = sys.argv[1]
    roson_file = open(roson_filename)
    roson = json.load(roson_file)

    controller = MockController(samples_file, robot_name, FearfulAlgorithm, roson)

    #import cProfile
    #cProfile.run('controller.loop()', '/tmp/profile.out')
    controller.loop()



