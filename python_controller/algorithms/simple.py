__author__ = 'michal'

import logging
import time
from robot_command import RobotCommand
from robot_model import VisState
from enum import Enum

class AlgorithmState(Enum):
    obtain_new_target = 1
    rotate = 2
    navigate = 3

class SimpleAlgorithm:

    INTERVAL = 0.02 #s
    TIME_MEASURE_STEPS = 50

    def __init__(self, controller, robot_name):
        self._controller = controller
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Simple algorithm started")
        self._robot_name = robot_name


        self._dir = 1

        self._state = AlgorithmState.obtain_new_target

        self._steps_counter = 0
        self._period_start = None
        self._step_start = None
        self._step_cumulative_time = 0.0

    def get_robot_name(self):
        return self._robot_name

    def loop(self):
        self._period_start = time.time()

        while True:
            self._step_start = time.time()
            states_dict = self._controller.request_states()

            if self._robot_name in states_dict:
                myrobot = states_dict[self._robot_name]

                vis_state = VisState(self._robot_name, states_dict)
                self._controller.send_vis_update(vis_state)

                speed = self._dir * 5
                self._controller.send_robot_command(RobotCommand(speed, speed, speed, speed))

            self._sleep_and_measure()

    def _sleep_and_measure(self):
        now = time.time()

        self._steps_counter += 1
        if self._steps_counter == self.TIME_MEASURE_STEPS:
            avg_period = (now - self._period_start) / float(self.TIME_MEASURE_STEPS) * 1000.0
            avg_step = self._step_cumulative_time / float(self.TIME_MEASURE_STEPS) * 1000.0
            self._step_cumulative_time = 0.0
            
            self._logger.info("avg period: %f ms" % avg_period)
            self._logger.info("avg step: %f ms" % avg_step)

            self._steps_counter = 0
            self._period_start = now

        exec_time = now - self._step_start
        self._step_cumulative_time += exec_time
        time_diff = self.INTERVAL - exec_time
        if time_diff > 0:
            time.sleep(time_diff)