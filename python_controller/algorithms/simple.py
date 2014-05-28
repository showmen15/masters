from algorithms.abstract_algorithm import AbstractAlgorithm
import random

__author__ = 'michal'

import math
from robot_command import RobotCommand
from robot_model import RobotConstants
from utils.prediction_utils import PredictionUtils

from utils.measurement_utils import MeasurementUtils

class SimpleAlgorithm(AbstractAlgorithm):
    MAX_SPEED = 0.5
    ANGLE_MEASURE_STEPS = 5

    CIRCLES_RADIUS = 0.5

    def __init__(self, controller, robot_name):
        super(SimpleAlgorithm, self).__init__(controller, robot_name)
        self._logger.info("Simple algorithm started")

        self._target = None
        self._last_params = None

    def reset(self):
        super(SimpleAlgorithm, self).reset()

        self._target = None
        self._last_params = None

    def _loop(self):
        self._navigate()

    def _navigate(self):

        #if self._robot_name == "robot2":
        #    self._send_stop_command()
        #    return None

        dist = None

        while True:

            if self._target is not None:
                dist = self._target_distance()
                if dist >= 0.05:
                    break

            self._controller.obtain_new_target()
            self._target = self._controller.get_target()


        #if self._robot_name == "robot1":
        #    self._target = (0.0, 0.0)
        #elif self._robot_name == "robot2":
        #    self._target = (2.8, 2.8)

        x = self._own_robot['x']
        y = self._own_robot['y']
        theta = self._own_robot['theta']
        tx, ty = self._target
        bearing = MeasurementUtils.angle(x, y, tx, ty)

        v = SimpleAlgorithm.MAX_SPEED


        preds = PredictionUtils.predict_positions(x, y, v, bearing, 0.0)
        preds = SimpleAlgorithm._cut_predictions(preds, self._target)

        self._predictions[self._robot_name] = preds
        self._variables['rate'] = self._rate_predictions(preds)

        target_dist = MeasurementUtils.distance(x, y, tx, ty)

        if not self._find_intersections(preds, self.CIRCLES_RADIUS, 0):
            v, omega = SimpleAlgorithm._navigate_fun(v, theta, bearing)

            if target_dist < 1.0:
                v *= target_dist

            Vl, Vr = SimpleAlgorithm._get_wheel_speeds(v, omega)

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
            best_preds = SimpleAlgorithm._cut_predictions(best_preds, best_midpoint)

            best_rate = self._rate_predictions(best_preds)
            #print "last: %f %f %f - %f %f %f" % (best_v, best_bearing, best_rate, x, y, v)

            self._logger.info("ABC last new best rate: %f" % (best_rate, ))

            if self._find_intersections(best_preds, self.CIRCLES_RADIUS, 1):
                self._logger.info("ABC INTER")
                best_v = None
                best_bearing = None
                best_midpoint = None
                best_rate = None
                best_preds = None

            #print "last best_rate" + str(best_rate)

        for i in range(50):
            rand_v = random.uniform(0.1, self.MAX_SPEED * 2.0)
            rand_bearing = random.uniform(-math.pi, math.pi)
            rand_radius = random.uniform(0.0, self.MAX_SPEED * PredictionUtils.CIRCLES_DELTA * PredictionUtils.CIRCLES_NUM)

            mp_x = rand_radius * math.cos(rand_bearing)
            mp_y = rand_radius * math.sin(rand_bearing)
            rand_midpoint = mp_x, mp_y

            rand_preds = PredictionUtils.predict_positions(x, y, rand_v, rand_bearing, 0.0)
            rand_preds = SimpleAlgorithm._cut_predictions(rand_preds, rand_midpoint)
            rand_rate = self._rate_predictions(rand_preds)

            #print "%f %f %f %s" % (rand_v, rand_bearing, rand_rate, rand_midpoint)

            if self._find_intersections(rand_preds, self.CIRCLES_RADIUS * 2, 1):
                continue

            if best_rate is None or rand_rate < best_rate - 0.5:
                if best_rate is not None:
                    self._logger.info("ABC old best: %f, new best: %f" % (best_rate, rand_rate))

                best_v, best_bearing, best_preds, best_rate, best_midpoint \
                    = rand_v, rand_bearing, rand_preds, rand_rate, rand_midpoint
                self._last_params = best_v, best_bearing, best_midpoint
                #print "new: %f %f %f %s" % (best_v, best_bearing, best_rate, best_preds)

        if best_preds is None:
            self._send_stop_command()
            return
        else:
            self._predictions[self._robot_name] = best_preds
            self._variables['rate'] = self._rate_predictions(best_preds)

            best_v, omega = SimpleAlgorithm._navigate_fun(best_v, theta, best_bearing)

            mp_x, mp_y = best_midpoint
            midpoint_dist = MeasurementUtils.distance(x, y, mp_x, mp_y)

            if midpoint_dist < 0.5:
                best_v *= midpoint_dist

            Vl, Vr = SimpleAlgorithm._get_wheel_speeds(best_v, omega)

            #self._logger.info("ABC v: %f, omega: %f, rate: %f, vl: %f,  vr: %f" % (best_v, best_omega, best_rate, Vl, Vr))

            self._controller.send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

    @staticmethod
    def _navigate_fun(v, theta, target_theta):
        angle = theta - target_theta
        if abs(angle) > math.pi:
            angle -= math.copysign(2.0 * math.pi, angle)

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

            if AbstractAlgorithm.get_ff(robot_name) < my_ff:
                continue

            for ((mx, my), (ox, oy)) in zip(own_predictions[ignored_num:], other_predictions[ignored_num:]):
                if MeasurementUtils.distance(mx, my, ox, oy) <= radius:
                    return True

        return False