from algorithms.abstract_algorithm import AbstractAlgorithm

__author__ = 'michal'

import math
from robot_command import RobotCommand
from robot_model import RobotConstants
from utils.prediction_utils import PredictionUtils

from utils.measurement_utils import MeasurementUtils

class GoAndTurn(AbstractAlgorithm):
    MAX_SPEED = 0.5
    ANGLE_MEASURE_STEPS = 5

    CIRCLES_RADIUS = 0.5

    def __init__(self, controller, robot_name):
        super(GoAndTurn, self).__init__(controller, robot_name)
        self._logger.info("GoAndTurn algorithm started")

        self._target = None
        self._counter = 300

    def reset(self):
        super(GoAndTurn, self).reset()

        self._target = None

    def _loop(self):
        self._navigate()

    def _navigate(self):

        if self._counter == 0:
            Vl = 1.0
            Vr = 0.5
        elif self._counter > 0:
            Vl = 1.0
            Vr = 1.0
            self._counter -= 1


        x = self._own_robot['x']
        y = self._own_robot['y']
        theta = self._own_robot['theta']

        v = 0.5 * (Vr + Vl)
        omega = (Vr - Vl) / RobotConstants.ROBOT_WIDTH

        own_predictions = PredictionUtils.predict_positions(x, y, v, theta, omega)
        self._predictions[self._robot_name] = own_predictions

        self._controller.send_robot_command(
            RobotCommand(Vl, Vr, Vl, Vr))

        #print Vl, Vr, Vl, Vr