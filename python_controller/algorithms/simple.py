from algorithms.abstract_algorithm import AbstractAlgorithm

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

    def reset(self):
        super(SimpleAlgorithm, self).reset()

        self._target = None

    def _loop(self):
        self._navigate()

    def _navigate(self):
        dist = None

        while True:

            if self._target is not None:
                dist = self._target_distance()
                if dist >= 0.05:
                    break

            self._controller.obtain_new_target()
            self._target = self._controller.get_target()

        angle = self._own_robot['theta'] - self._target_angle()
        if abs(angle) > math.pi:
            angle -= math.copysign(2.0 * math.pi, angle)

        p = angle / math.pi

        correction = self.MAX_SPEED * p * 2.0
        speed = self.MAX_SPEED

        if dist < 1.0:
            speed *= dist

        l_speed = speed + correction
        r_speed = speed - correction

        if self._find_intersections(l_speed, r_speed):
            self._send_stop_command()
            return

        self._controller.send_robot_command(
            RobotCommand(l_speed, r_speed, l_speed, r_speed))

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

    def _find_intersections(self, Vl, Vr):
        x = self._own_robot['x']
        y = self._own_robot['y']
        theta = self._own_robot['theta']

        v = 0.5 * (Vr + Vl)
        omega = (Vr - Vl) / RobotConstants.ROBOT_WIDTH

        own_predictions = PredictionUtils.predict_positions(x, y, v, theta, omega)

        (tx, ty) = self._target

        for i in range(len(own_predictions)):
            (px, py) = own_predictions[i]
            if MeasurementUtils.distance(px, py, tx, ty) < 0.3:
                own_predictions = own_predictions[:i+1]
                break

        self._predictions[self._robot_name] = own_predictions

        my_ff = AbstractAlgorithm.get_ff(self._robot_name)

        for (robot_name, other_predictions) in self._predictions.items():
            if self._robot_name == robot_name:
                continue

            if AbstractAlgorithm.get_ff(robot_name) < my_ff:
                continue

            for ((mx, my), (ox, oy)) in zip(own_predictions, other_predictions):
                if MeasurementUtils.distance(mx, my, ox, oy) <= self.CIRCLES_RADIUS:
                    return True

        return False





