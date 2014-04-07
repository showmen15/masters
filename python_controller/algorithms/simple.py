__author__ = 'michal'

import logging
import time
from robot_command import RobotCommand


class SimpleAlgorithm:

    def __init__(self):
        self._logger = logging.getLogger('SimpleAlgorithm')
        self._robot_name = None
        self._counter = 0
        self._before = time.time()
        self._start = None
        self._dir = 1

    def set_robot_name(self, robot_name):
        self._robot_name = robot_name

    def react(self, states_dict):
        myrobot = states_dict[0]

        if self._start is None:
            self._start = myrobot.get_timestamp()

        self._counter += 1
        if self._counter == 1000:
            self._counter = 0
            t = time.time() - self._before
            self._before = time.time()
            self._logger.info("time: %f, %d" % (t, myrobot.get_timestamp()))

        if myrobot.get_timestamp() - self._start > 1000*1000*10:
            self._start = myrobot.get_timestamp()
            self._dir *= -1

        speed = self._dir * 10

        return RobotCommand(speed, speed, speed, speed)


