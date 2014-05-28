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

        if self._robot_name == "robot2":
            self._send_stop_command()
            return None

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

        v, omega = SimpleAlgorithm._navigate_fun(x, y, theta, self._target)

        best_v = v
        best_omega = omega

        best_preds = PredictionUtils.predict_positions_with_fun(x, y, theta, self._target, SimpleAlgorithm._navigate_fun)
        best_preds = self._cut_predictions(best_preds)


        best_rate = 0.0

        self._predictions[self._robot_name] = best_preds
        self._variables['rate'] = self._rate_predictions(best_preds)

        if self._find_intersections(best_preds, self.CIRCLES_RADIUS):
            best_v = None
            best_omega = None
            best_rate = None
            best_preds = None

            if self._last_params is not None:
                best_v, best_omega = self._last_params
                best_preds = self._cut_predictions(PredictionUtils.predict_positions(x, y, best_v, theta, best_omega))
                best_rate = self._rate_predictions(best_preds)

                self._logger.info("ABC last new best rate: %f" % (best_rate))

                if self._find_intersections(best_preds, self.CIRCLES_RADIUS):
                    self._logger.info("ABC INTER")
                    best_v = None
                    best_omega = None
                    best_rate = None
                    best_preds = None

                #print "last best_rate" + str(best_rate)

            for i in range(50):
                rand_v = random.uniform(-self.MAX_SPEED, self.MAX_SPEED)
                rand_omega = random.uniform(-10.0, 10.0)

                rand_preds = self._cut_predictions(PredictionUtils.predict_positions(x, y, rand_v, theta, rand_omega))
                rand_rate = self._rate_predictions(rand_preds)

                #print "   rand_rate " + str(rand_rate)

                if self._find_intersections(rand_preds, self.CIRCLES_RADIUS * 2):
                    continue

                if best_rate is None or rand_rate < best_rate - 0.5:
                    if best_rate is not None:
                        self._logger.info("ABC old best: %f, new best: %f" % (best_rate, rand_rate))

                    best_v, best_omega, best_preds, best_rate = rand_v, rand_omega, rand_preds, rand_rate
                    self._last_params = best_v, best_omega

        else:
            self._last_params = None

        if best_preds is None:
            self._send_stop_command()
            return
        else:
            self._predictions[self._robot_name] = best_preds
            self._variables['rate'] = self._rate_predictions(best_preds)

            Vr = best_v + 0.5 * RobotConstants.ROBOT_WIDTH * best_omega
            Vl = 2 * best_v - Vr

            #self._logger.info("ABC v: %f, omega: %f, rate: %f, vl: %f,  vr: %f" % (best_v, best_omega, best_rate, Vl, Vr))

            self._controller.send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

    @staticmethod
    def _navigate_fun(x, y, theta, target):
        tx, ty = target
        target_dist = MeasurementUtils.distance(x, y, tx, ty)
        target_angle = MeasurementUtils.angle(x, y, tx, ty)

        angle = theta - target_angle
        if abs(angle) > math.pi:
            angle -= math.copysign(2.0 * math.pi, angle)

        p = angle / math.pi

        omega = -10.0 * p * 2.0
        omega = math.copysign(min(abs(omega), 5.0), omega)

        v = SimpleAlgorithm.MAX_SPEED

        if target_dist < 1.0:
            v *= target_dist

        return v, omega


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

    def _cut_predictions(self, predictions):
        (tx, ty) = self._target

        for i in range(len(predictions)):
            (px, py) = predictions[i]
            if MeasurementUtils.distance(px, py, tx, ty) < 0.3:
                return predictions[:i+1]

        return predictions

    def _rate_predictions(self, predictions):
        assert len(predictions) > 0

        (tx, ty) = self._target
        (px, py) = predictions[-1]

        return MeasurementUtils.distance(px, py, tx, ty)

    def _find_intersections(self, own_predictions, radius):
        assert len(own_predictions) > 0

        my_ff = AbstractAlgorithm.get_ff(self._robot_name)

        for (robot_name, other_predictions) in self._predictions.items():
            if self._robot_name == robot_name:
                continue

            if AbstractAlgorithm.get_ff(robot_name) < my_ff:
                continue

            for ((mx, my), (ox, oy)) in zip(own_predictions, other_predictions):
                if MeasurementUtils.distance(mx, my, ox, oy) <= radius:
                    return True

        return False