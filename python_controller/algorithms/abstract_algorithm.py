__author__ = 'michal'

import logging
import math

from robot_model import VisState
from robot_command import RobotCommand
from utils.time_utils import TimeUtil
from utils.prediction_utils import PredictionUtils
from kalman.location_kalman import LocationKalman
from kalman.angle_kalman import AngleKalman


class AbstractAlgorithm(object):
    SAVE_STATES = False

    INTERVAL = 0.02 #s
    MEASURE_STEPS = 50 * 5 # 5s

    def __init__(self, controller, robot_name):
        self._logger = logging.getLogger(robot_name)

        self._controller = controller
        self._robot_name = robot_name

        self._time_util = TimeUtil(self.INTERVAL, self.MEASURE_STEPS, self._logger)

        self._states = {}
        self._predictions = None
        self._own_robot = None
        self._running = True

        if self.SAVE_STATES:
            self._f = open("/tmp/%s.states" % (self._robot_name, ), 'w')
        else:
            self._f = None

    def get_robot_name(self):
        return self._robot_name

    def reset(self):
        self._logger.info("Reset")

        self._states = {}
        self._predictions = None
        self._own_robot = None
        self._running = True

    def start(self):
        self._logger.info("Start")
        self._running = True

    def stop(self):
        self._logger.info("Stop")
        self._running = False

    def loop(self):
        while True:
            self._time_util.start_step()

            if self._update_states():
                self._predict()
                self._loop()
                self._send_vis_state()

            self._time_util.start_sleep()

    def _loop(self):
        raise NotImplemented()

    @staticmethod
    def get_ff(robot_name):
        return int(robot_name[5:])

    def _send_stop_command(self):
        self._controller.send_robot_command(RobotCommand(0, 0, 0, 0))

    def _send_vis_state(self):

        states_to_send = {}
        for (robot_name, state) in self._states.items():
            states_to_send[robot_name] = (state['x'], state['y'], state['theta'])

        vis_state = VisState(self._robot_name, states_to_send)
        vis_state.set_target(self._target)
        vis_state.set_predictions(self._predictions)

        self._controller.send_vis_update(vis_state)

    def _update_states(self):
        new_states = self._controller.request_states()

        if not self._running or len(new_states) == 0:
            return False

        for (robot_name, new_state) in new_states.items():
            org_x = new_state.get_x()
            org_y = new_state.get_y()
            org_theta = new_state.get_theta()
            timestamp = new_state.get_timestamp()

            if robot_name not in self._states:
                location_kalman = LocationKalman(org_x, org_y, self.INTERVAL)
                angle_kalman = AngleKalman(org_theta, self.INTERVAL)

                state = {'location_kalman': location_kalman,
                         'angle_kalman': angle_kalman,
                         'timestamp': timestamp,
                         'x': org_x,
                         'y': org_y,
                         'vx': 0.0,
                         'vy': 0.0,
                         'v': 0.0,
                         'theta': org_theta,
                         'omega': 0.0,
                         'epsilon': 0.0}

                self._states[robot_name] = state

            state = self._states[robot_name]

            location_kalman = state['location_kalman']
            angle_kalman = state['angle_kalman']

            if timestamp > state['timestamp']:
                location_kalman.step(org_x, org_y)
                angle_kalman.step(org_theta)
            else:
                location_kalman.missing_step()
                angle_kalman.missing_step()

            (x, y, vx, vy) = location_kalman.get_means()
            (theta, omega, epsilon) = angle_kalman.get_means()

            v = math.sqrt(vx * vx + vy * vy)

            state['x'] = x
            state['y'] = y
            state['vx'] = vx
            state['vy'] = vy
            state['v'] = v
            state['theta'] = theta
            state['omega'] = omega
            state['epsilon'] = epsilon
            state['timestamp'] = timestamp

            if self._f is not None and robot_name == self._robot_name:
                self._f.write("%i %f %f %f %f %f %f %f %f\n" %
                              (timestamp, x, org_x, y, org_y, v, theta, org_theta, omega))

        if self._robot_name not in self._states:
            return False

        self._own_robot = self._states[self._robot_name]

        return True

    def _predict(self):
        self._predictions = {}

        for (robot_name, state) in self._states.items():
            if self._robot_name == robot_name:
                continue

            predictions = PredictionUtils.predict_positions(state['x'], state['y'], state['v'],
                                                            state['theta'],
                                                            state['omega'])
            self._predictions[robot_name] = predictions