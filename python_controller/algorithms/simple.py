__author__ = 'michal'

import logging
import time
import random
import math
from robot_command import RobotCommand
from robot_model import VisState
from utils.time_utils import TimeUtil
from enum import Enum
from kalman.location_kalman import LocationKalman
from kalman.angle_kalman import AngleKalman


class AlgorithmState(Enum):
    obtain_new_target = 1
    rotate = 2
    navigate = 3


class SimpleAlgorithm:
    INTERVAL = 0.02 #s
    MEASURE_STEPS = 50 * 5 # 5s
    MAX_SPEED = 0.5
    ANGLE_MEASURE_STEPS = 5

    MIN_X = -5.0
    MIN_Y = -5.0

    WORLD_WIDTH = 10.0
    WORLD_HEIGHT = 10.0

    WALL_OFFSET = 1.0

    CIRCLES_DELTA = 0.5
    CIRCLES_NUM = 10
    CIRCLES_RADIUS = 0.5

    def __init__(self, controller, robot_name):
        self._logger = logging.getLogger(robot_name)
        self._logger.info("Simple algorithm started")

        self._controller = controller
        self._robot_name = robot_name
        self._time_util = TimeUtil(self.INTERVAL, self.MEASURE_STEPS, self._logger)

        self._state = AlgorithmState.obtain_new_target
        self._target = None
        self._states_dict = {}
        self._my_robot = None
        self._prev_states_dict = {}
        self._movements = {}
        self._circles_dict = None
        self._kf_dict = {}
        self._angle_kf_dict = {}
        self._running = True

        #self._f = open("/tmp/%s.ang" % (self._robot_name, ), 'w')

        self._a = None
        self._b = None

    def get_robot_name(self):
        return self._robot_name

    def loop(self):
        while True:
            self._time_util.start_step()

            self._update_states()
            if self._my_robot is not None:
                self._calculates_movements()
                self._generate_circles()
                self._make_action()

            self._time_util.start_sleep()

    def reset(self):
        self._logger.info("Reset")

        self._states_dict.clear()
        self._prev_states_dict.clear()
        self._my_robot = None
        self._movements.clear()
        self._circles_dict = None
        self._target = None
        self._state = AlgorithmState.obtain_new_target
        self._kf_dict = {}
        self._angle_kf_dict = {}
        self._running = True

    def start(self):
        self._logger.info("Start")
        self._running = True

    def stop(self):
        self._logger.info("Stop")
        self._running = False
        #self._f.close()

    def _update_states(self):
        new_states = self._controller.request_states()

        if not self._running:
            return

        for (robot_name, new_state) in new_states.items():
            if robot_name in self._states_dict:
                old_state = self._states_dict[robot_name]

                if new_state.get_timestamp() > old_state.get_timestamp():
                    self._prev_states_dict[robot_name] = old_state
                    self._states_dict[robot_name] = new_state
                    self._kf_dict[robot_name].step(new_state.get_x(), new_state.get_y())
                    self._angle_kf_dict[robot_name].step(new_state.get_theta())

                    if robot_name == self._robot_name:
                        self._f.write("%f\n" % (new_state.get_theta(), ))
                else:
                    self._kf_dict[robot_name].missing_step()
                    self._angle_kf_dict[robot_name].missing_step()

            else:
                self._states_dict[robot_name] = new_state
                self._kf_dict[robot_name] = LocationKalman(new_state.get_x(), new_state.get_y(),
                                                           self.INTERVAL)
                self._angle_kf_dict[robot_name] = AngleKalman(new_state.get_theta(), self.INTERVAL)

                if robot_name in self._prev_states_dict:
                    del self._prev_states_dict[robot_name]

        if self._robot_name in self._states_dict:
            self._my_robot = self._states_dict[self._robot_name]

    def _make_action(self):
        while True:
            if self._state == AlgorithmState.obtain_new_target:
                self._obtain_new_target()
                self._state = AlgorithmState.navigate
            elif self._state == AlgorithmState.rotate:
                self._rotate()
                break
            elif self._state == AlgorithmState.navigate:
                self._navigate()
                break

        self._send_vis_state()

    def _send_vis_state(self):
        vis_state = VisState(self._robot_name, self._states_dict)
        vis_state.set_target(self._target)
        vis_state.set_circles(self._circles_dict)
        vis_state.add_variable("angle", self._a)
        vis_state.add_variable("ang_speed", self._b)

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
        p = angle / math.pi

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

        p = angle / math.pi

        correction = self.MAX_SPEED * p * 2.0
        speed = self.MAX_SPEED

        if dist < 1.0:
            speed *= dist

        if self._find_intersections(speed):
            self._send_stop_command()
            return
            #pass

        self._controller.send_robot_command(
            RobotCommand(speed - correction, speed + correction, speed - correction, speed + correction))

    def _send_stop_command(self):
        self._controller.send_robot_command(RobotCommand(0, 0, 0, 0))

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

                angle = act_state.get_theta()
                (x, y, vx, vy) = self._kf_dict[robot_name].get_means()
                (a, o, e) = self._angle_kf_dict[robot_name].get_means()

                if robot_name == self._robot_name:
                    self._a = a
                    self._b = round(o, 2)
                    #self._f.write("%f %f\n" % (a, o))

                speed = math.sqrt(vx * vx + vy * vy)
                act_point = (x, y)

                self._movements[robot_name] = {'speed': speed, 'angle': a, 'ang_speed': o,
                                               'act_point': act_point}

    @staticmethod
    def _distance(x1, y1, x2, y2):
        a = abs(x1 - x2)
        b = abs(y1 - y2)

        return math.sqrt(a * a + b * b)

    @staticmethod
    def _angle(x1, y1, x2, y2):
        x = x2 - x1
        y = y2 - y1

        return math.atan2(x, y)

    @staticmethod
    def _get_ff(robot_name):
        return int(robot_name[5:])

    def _generate_circles(self):
        self._circles_dict = {}

        for (robot_name, movements) in self._movements.items():
            if self._robot_name == robot_name:
                continue

            circles = self._generate_circles_for_one(movements['act_point'], movements['speed'], movements['angle'])
            self._circles_dict[robot_name] = circles

    def _find_intersections(self, desired_speed):
        if self._robot_name not in self._movements:
            return False

        my_movements = self._movements[self._robot_name]
        my_circles = self._generate_circles_for_one(my_movements['act_point'], desired_speed, my_movements['angle'])
        self._circles_dict[self._robot_name] = my_circles

        my_ff = SimpleAlgorithm._get_ff(self._robot_name)

        for (robot_name, circles) in self._circles_dict.items():
            if self._robot_name == robot_name:
                continue

            if SimpleAlgorithm._get_ff(robot_name) < my_ff:
                continue

            assert len(my_circles) == self.CIRCLES_NUM and len(circles) == self.CIRCLES_NUM
            for i in range(self.CIRCLES_NUM):
                (mx, my) = my_circles[i]
                (ox, oy) = circles[i]

                if SimpleAlgorithm._distance(mx, my, ox, oy) <= self.CIRCLES_RADIUS:
                    return True

        return False

    def _generate_circles_for_one(self, act_point, speed, angle):
        z = speed * self.CIRCLES_DELTA
        (x, y) = act_point

        dx = z * math.sin(angle)
        dy = z * math.cos(angle)

        circles = [(x + dx * i, y + dy * i) for i in range(self.CIRCLES_NUM)]
        return circles