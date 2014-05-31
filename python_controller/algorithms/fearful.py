__author__ = 'michal'

import random
import time
import math
from robot_command import RobotCommand
from robot_model import RobotConstants
from utils.prediction_utils import PredictionUtils
from algorithms.abstract_algorithm import AbstractAlgorithm
from utils.measurement_utils import MeasurementUtils
from operator import itemgetter

class FearfulAlgorithm(AbstractAlgorithm):
    CRUISE_SPEED = 0.5
    ANGLE_MEASURE_STEPS = 5

    CIRCLES_RADIUS = 0.5

    VARIANTS_MAX_TIME = 0.015

    FF_RADIUS = 2.0

    def __init__(self, controller, robot_name):
        super(FearfulAlgorithm, self).__init__(controller, robot_name)
        self._logger.info("Simple algorithm started")

        self._target = None
        self._last_params = None

        self._fear_factors = None

    def reset(self):
        super(FearfulAlgorithm, self).reset()

        self._target = None
        self._last_params = None

        self._fear_factors = None

    def _loop(self):
        self._compute_fear_factors()
        self._navigate()

    def _compute_fear_factors(self):
        self._fear_factors = {}

        for robot_name, state in self._states.items():
            x = state['x']
            y = state['y']
            theta = state['theta']

            ff = AbstractAlgorithm.get_ff(robot_name)

            for robot_name2, state2 in self._states.items():
                if robot_name == robot_name2:
                    continue

                x2 = state2['x']
                y2 = state2['y']
                theta2 = state2['theta']

                dist = MeasurementUtils.distance(x, y, x2, y2)
                if dist > FearfulAlgorithm.FF_RADIUS:
                    continue

                angle_diff = abs(MeasurementUtils.normalize_angle(theta - theta2))

                if angle_diff > 0.5 * math.pi:
                    continue

                ff += (1 - dist / FearfulAlgorithm.FF_RADIUS) * math.cos(angle_diff) * AbstractAlgorithm.get_ff(robot_name2)

            self._fear_factors[robot_name] = ff

    def _modify_vis_state(self, vis_state):
        vis_state.set_fear_factors(self._fear_factors)

    def _navigate(self):
        dist = None

        while True:
            if self._target is not None:
                dist = self._target_distance()
                if dist >= 0.05:
                    break

            self._controller.obtain_new_target()
            self._target = self._controller.get_target()

        x = self._own_robot['x']
        y = self._own_robot['y']
        theta = self._own_robot['theta']
        v = FearfulAlgorithm.CRUISE_SPEED

        if self._avoid_close_robots(x, y, theta):
            return

        tx, ty = self._target
        bearing = MeasurementUtils.angle(x, y, tx, ty)

        preds = PredictionUtils.predict_positions(x, y, v, bearing, 0.0)
        preds = FearfulAlgorithm._cut_predictions(preds, self._target)

        self._predictions[self._robot_name] = preds
        self._variables['rate'] = self._rate_predictions(preds)

        target_dist = MeasurementUtils.distance(x, y, tx, ty)

        if not self._find_intersections(preds, self.CIRCLES_RADIUS * 2.0, 0):
            v, omega = FearfulAlgorithm._navigate_fun(v, theta, bearing, False)

            if target_dist < 0.5:
                v *= target_dist

            Vl, Vr = FearfulAlgorithm._get_wheel_speeds(v, omega)

            self._controller.send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

            return

        best_v = None
        best_bearing = None
        best_midpoint = None
        best_rate = None
        best_preds = None
        self._predictions[self._robot_name] = None

        if self._last_params is not None:
            best_v, best_bearing, best_midpoint = self._last_params

            best_preds = PredictionUtils.predict_positions(x, y, best_v, best_bearing, 0.0)
            best_preds = FearfulAlgorithm._cut_predictions(best_preds, best_midpoint)

            best_rate = self._rate_predictions(best_preds)

            if self._find_intersections(best_preds, self.CIRCLES_RADIUS, 1):
                best_v = None
                best_bearing = None
                best_midpoint = None
                best_rate = None
                best_preds = None

        variants_stop_time = time.time() + FearfulAlgorithm.VARIANTS_MAX_TIME

        while time.time() < variants_stop_time:
            rand_v = random.uniform(0.0, self.CRUISE_SPEED * 2.0)
            rand_bearing = random.uniform(-math.pi, math.pi)
            rand_radius = random.uniform(0.0, self.CRUISE_SPEED * PredictionUtils.CIRCLES_DELTA * PredictionUtils.CIRCLES_NUM)

            mp_x = rand_radius * math.cos(rand_bearing)
            mp_y = rand_radius * math.sin(rand_bearing)
            rand_midpoint = mp_x, mp_y

            rand_preds = PredictionUtils.predict_positions(x, y, rand_v, rand_bearing, 0.0)
            rand_preds = FearfulAlgorithm._cut_predictions(rand_preds, rand_midpoint)
            rand_rate = self._rate_predictions(rand_preds)

            if self._find_intersections(rand_preds, self.CIRCLES_RADIUS * 2, 1):
                continue

            if best_rate is None or rand_rate < best_rate - 0.5:
                best_v, best_bearing, best_preds, best_rate, best_midpoint \
                    = rand_v, rand_bearing, rand_preds, rand_rate, rand_midpoint
                self._last_params = best_v, best_bearing, best_midpoint

        if best_preds is None:
            self._send_stop_command()
            return
        else:
            self._predictions[self._robot_name] = best_preds
            self._variables['rate'] = self._rate_predictions(best_preds)

            best_v, omega = FearfulAlgorithm._navigate_fun(best_v, theta, best_bearing, False)

            mp_x, mp_y = best_midpoint
            midpoint_dist = MeasurementUtils.distance(x, y, mp_x, mp_y)

            if midpoint_dist < 0.5:
                best_v *= midpoint_dist

            Vl, Vr = FearfulAlgorithm._get_wheel_speeds(best_v, omega)

            self._controller.send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

    def _avoid_close_robots(self, x, y, theta):
        distances = self._distances_to_robots()
        if len(distances) > 0:
            robot_name, distance = distances[0]
            if distance < FearfulAlgorithm.CIRCLES_RADIUS:
                state = self._states[robot_name]
                rx = state['x']
                ry = state['y']

                bearing_to_robot = MeasurementUtils.angle(x, y, rx, ry)
                opposite_bearing = MeasurementUtils.normalize_angle(bearing_to_robot + math.pi)

                v, omega = FearfulAlgorithm._navigate_fun(FearfulAlgorithm.CRUISE_SPEED, theta, opposite_bearing, True)
                Vl, Vr = FearfulAlgorithm._get_wheel_speeds(v, omega)

                preds = PredictionUtils.predict_positions(x, y, v, opposite_bearing, 0.0)
                self._predictions[self._robot_name] = preds

                self._controller.send_robot_command(
                    RobotCommand(Vl, Vr, Vl, Vr))

                return True

        return False

    @staticmethod
    def _navigate_fun(v, theta, target_theta, with_reverse):
        angle = theta - target_theta
        angle = MeasurementUtils.normalize_angle(angle)

        if with_reverse and abs(angle) > 0.5 * math.pi:
            angle = math.copysign(math.pi - abs(angle), angle)
            v *= -1.0

        p = angle / math.pi

        omega = -10.0 * p * 2.0
        omega = math.copysign(min(abs(omega), 5.0), omega)

        if abs(omega) > 1.0:
            v = 0.0

        return v, omega



    @staticmethod
    def _get_wheel_speeds(v, omega):
        Vr = v + 0.5 * RobotConstants.ROBOT_WIDTH * omega
        Vl = 2 * v - Vr

        return Vl, Vr

    def _target_distance(self):
        assert self._target is not None

        (tx, ty) = self._target
        mx = self._own_robot['x']
        my = self._own_robot['y']

        return MeasurementUtils.distance(tx, ty, mx, my)

    def _target_angle(self):
        assert self._target is not None

        (tx, ty) = self._target
        mx = self._own_robot['x']
        my = self._own_robot['y']

        return MeasurementUtils.angle(mx, my, tx, ty)

    @staticmethod
    def _cut_predictions(predictions, target):
        (tx, ty) = target

        for i in range(len(predictions)):
            (px, py) = predictions[i]
            if MeasurementUtils.distance(px, py, tx, ty) < 0.3:
                return predictions[:i+1] + [(tx, ty)] * (len(predictions) - i + 1)

        return predictions

    def _distances_to_robots(self):
        distances = []

        x = self._own_robot['x']
        y = self._own_robot['y']

        for robot_name, state in self._states.items():
            if robot_name == self._robot_name:
                continue

            rx = state['x']
            ry = state['y']
            distance = MeasurementUtils.distance(x, y, rx, ry)

            distances.append((robot_name, distance))

        distances.sort(key=itemgetter(1))
        return distances

    def _rate_predictions(self, predictions):
        assert len(predictions) > 0

        (tx, ty) = self._target
        (px, py) = predictions[-1]

        return MeasurementUtils.distance(px, py, tx, ty)

    def _find_intersections(self, own_predictions, radius, ignored_num):
        assert len(own_predictions) > 0

        my_ff = AbstractAlgorithm.get_ff(self._robot_name)

        for (robot_name, other_predictions) in self._predictions.items():
            if self._robot_name == robot_name:
                continue

            if self._fear_factors[self._robot_name] > self._fear_factors[robot_name]:
                continue

            for ((mx, my), (ox, oy)) in zip(own_predictions[ignored_num:], other_predictions[ignored_num:]):
                if MeasurementUtils.distance(mx, my, ox, oy) <= radius:
                    return True

        return False