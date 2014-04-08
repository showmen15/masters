__author__ = 'michal'

import logging
import time
from robot_command import RobotCommand


class SimpleAlgorithm:

    INTERVAL = 0.02 #s

    def __init__(self, controller, robot_name):
        self._controller = controller
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Simple algorithm started")
        self._robot_name = robot_name
        self._counter = 0
        self._before = time.time()
        self._start = None
        self._dir = 1

    def get_robot_name(self):
        return self._robot_name

    def loop(self):
        while True:
            start_time = time.time()
            states = self._controller.request_states()
            myrobot = states[0]

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

            self._controller.send_robot_command(RobotCommand(speed, speed, speed, speed))

            time_diff = self.INTERVAL - (time.time() - start_time)
            if time_diff > 0:
                time.sleep(time_diff)


