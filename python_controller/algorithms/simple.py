__author__ = 'michal'

import logging
import time
import random
import math
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
    MAX_SPEED = 10

    MIN_X = -5.0
    MIN_Y = -5.0

    WORLD_WIDTH = 10.0
    WORLD_HEIGHT = 10.0

    WALL_OFFSET = 0.5

    CIRCLES_DELTA = 1.0
    CIRCLES_NUM = 10

    def __init__(self, controller, robot_name):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Simple algorithm started")

        self._controller = controller
        self._robot_name = robot_name

        self._steps_counter = 0
        self._period_start = None
        self._step_start = None
        self._step_cumulative_time = 0.0

        self._state = AlgorithmState.obtain_new_target
        self._target = None
        self._states_dict = {}
        self._my_robot = None
        self._prev_states_dict = {}
        self._movements = {}
        self._circles_dict = None

        self._a = None

    def get_robot_name(self):
        return self._robot_name

    def loop(self):
        self._period_start = time.time()

        while True:
            self._step_start = time.time()

            self._update_states()
            self._calculates_movements()
            self._generate_circles()
            self._make_action()

            self._sleep_and_measure()

    def _update_states(self):
        new_states = self._controller.request_states()

        for (robot_name, new_state) in new_states.items():
            if robot_name in self._states_dict:
                old_state = self._states_dict[robot_name]
                if new_state.get_timestamp() > old_state.get_timestamp():
                    self._prev_states_dict[robot_name] = old_state
                    self._states_dict[robot_name] = new_state
            else:
                self._states_dict[robot_name] = new_state

        assert self._robot_name in self._states_dict
        self._my_robot = self._states_dict[self._robot_name]



    def _make_action(self):
        while True:
            if self._state == AlgorithmState.obtain_new_target:
                self._obtain_new_target()
                self._state = AlgorithmState.rotate
            elif self._state == AlgorithmState.rotate:
                self._rotate()
                break
            elif self._state == AlgorithmState.navigate:
                self._navigate()
                break;

        self._send_vis_state()


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

    def _send_vis_state(self):
        vis_state = VisState(self._robot_name, self._states_dict)
        vis_state.set_target(self._target)
        vis_state.set_circles(self._circles_dict)
        vis_state.add_variable("angle", self._a)

        self._controller.send_vis_update(vis_state)

    def _obtain_new_target(self):
        target_x = random.uniform(self.MIN_X + self.WALL_OFFSET,
                                  self.MIN_X + self.WORLD_WIDTH - self.WALL_OFFSET)

        target_y = random.uniform(self.MIN_Y + self.WALL_OFFSET,
                                  self.MIN_Y + self.WORLD_HEIGHT - self.WALL_OFFSET)

        self._target = (target_x, target_y)
        self._logger.info("New target obtained: %s, switching to rotate state",
                          (self._target, ))

    def _rotate(self):
        angle = self._my_robot.get_theta() - self._target_angle()
        self._a = angle
        p = angle/math.pi

        if abs(p) < 0.01:
            self._state = AlgorithmState.navigate
            return

        speed = self.MAX_SPEED * p
        self._controller.send_robot_command(RobotCommand(-speed, speed, -speed, speed))

    def _navigate(self):
        dist = self._target_distance()
        if dist < 0.05:
            self._state = AlgorithmState.obtain_new_target
            return

        angle = self._my_robot.get_theta() - self._target_angle()
        if abs(angle) > math.pi:
            angle -= math.copysign(2.0 * math.pi, angle)

        self._a = angle
        p = angle/math.pi

        correction = self.MAX_SPEED * p
        speed = self.MAX_SPEED

        if dist < 1.0:
            speed *= dist

        self._controller.send_robot_command(
            RobotCommand(speed - correction, speed + correction, speed - correction, speed + correction))

    def _target_distance(self):
        assert self._target is not None

        (tx, ty) = self._target
        mx = self._my_robot.get_x()
        my = self._my_robot.get_y()

        return SimpleAlgorithm._distance(tx, ty, mx, my)

    def _target_angle(self):
        assert self._target is not None

        (tx, ty) = self._target
        mx = self._my_robot.get_x()
        my = self._my_robot.get_y()

        return SimpleAlgorithm._angle(mx, my, tx, ty)

    def _calculates_movements(self):
        self._movements.clear()

        for (robot_name, act_state) in self._states_dict.items():
            if robot_name in self._prev_states_dict:
                prev_state = self._prev_states_dict[robot_name]

                delta = (act_state.get_timestamp() - prev_state.get_timestamp()) / (1000.0 * 1000.0)
                assert delta > 0.0

                distance = SimpleAlgorithm._distance(act_state.get_x(), act_state.get_y(),
                                                     prev_state.get_x(), prev_state.get_y())

                speed = distance / delta
                angle = SimpleAlgorithm._angle(prev_state.get_x(), prev_state.get_y(),
                                               act_state.get_x(), act_state.get_y())

                act_point = (act_state.get_x(), act_state.get_y())

                self._movements[robot_name] = {'speed': speed, 'distance': distance, 'angle': angle,
                                               'act_point': act_point}

    @staticmethod
    def _distance(x1, y1, x2, y2):
        a = abs(x1 - x2)
        b = abs(y1 - y2)

        return math.sqrt(a*a + b*b)

    @staticmethod
    def _angle(x1, y1, x2, y2):
        x = x2 - x1
        y = y2 - y1

        return math.atan2(x, y)

    def _generate_circles(self):
        self._circles_dict = {}

        for (robot_name, movements) in self._movements.items():
            z = movements['speed'] * self.CIRCLES_DELTA
            (x, y) = movements['act_point']
            angle = movements['angle']

            dx = z * math.sin(angle)
            dy = z * math.cos(angle)

            circles = [(x + dx * i, y + dy * i) for i in range(1, self.CIRCLES_NUM + 1)]
            self._circles_dict[robot_name] = circles

