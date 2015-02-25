__author__ = 'michal'

import logging
import math
import time
import random

from collections import deque
from robot_model import VisState, StateMsg, StateMsgType
from robot_command import RobotCommand
from utils.time_utils import TimeUtil
from utils.measurement_utils import MeasurementUtils
from utils.prediction_utils import PredictionUtils
from kalman.location_kalman import LocationKalman
from kalman.angle_kalman import AngleKalman


class AbstractAlgorithm(object):
    SAVE_STATES = False

    INTERVAL = 0.6  # s
    MEASURE_STEPS = 50 * 5  # 5s
    AVG_TIMES_NUM = 10

    def __init__(self, controller, robot_name):
        self._logger = logging.getLogger(robot_name)

        self._controller = controller
        self._robot_name = robot_name

        self._time_util = TimeUtil(self.INTERVAL, self.MEASURE_STEPS, self._logger)

        self._states = {}
        self._predictions = None
        self._own_robot = None
        self._running = True
        self._variables = {}
        self._own_fear_factor = 0.0
        self._odometer = 0.0

        self._base_fear_factor = AbstractAlgorithm._random_base_fear_factor()

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
        self._variables = {}
        self._own_fear_factor = 0.0
        self._odometer = 0.0

        self._base_fear_factor = AbstractAlgorithm._random_base_fear_factor()

    def start(self):
        self._logger.info("Start")
        self._running = True
        self.send_start_msg()

    def stop(self):
        self._logger.info("Stop")
        self._running = False

    def loop(self):
        update_state_times = deque(maxlen=self.AVG_TIMES_NUM)
        predict_times = deque(maxlen=self.AVG_TIMES_NUM)
        # loop_times = deque(maxlen=self.AVG_TIMES_NUM)
        send_vis_times = deque(maxlen=self.AVG_TIMES_NUM)

        update_state_avg = 0.0
        predict_avg = 0.0
        #loop_avg = 0.0
        send_vis_avg = 0.0

        while True:
            self._time_util.start_step()

            t1 = time.time()
            state_changed = self._update_states()
            t2 = time.time()
            update_state_times.appendleft(t2 - t1)
            update_state_avg = sum(update_state_times) / float(len(update_state_times))

            if state_changed:
                self._predict()
                t3 = time.time()
                predict_times.appendleft(t3 - t2)
                predict_avg = sum(predict_times) / float(len(predict_times))

                avail_time = max(0.0, self.INTERVAL - update_state_avg - predict_avg - send_vis_avg)
                self._loop(avail_time)
                t4 = time.time()
                #loop_times.appendleft(t3 - t2)
                #loop_avg = sum(loop_times) / float(len(loop_times))

                self._send_vis_state()
                t5 = time.time()
                send_vis_times.appendleft(t5 - t4)
                send_vis_avg = sum(send_vis_times) / float(len(send_vis_times))

            self._time_util.start_sleep()

    def _loop(self, avail_time):
        raise NotImplemented()

    def _send_stop_command(self):
        self._send_robot_command(RobotCommand(0, 0, 0, 0))

    def _send_robot_command(self, robot_command):
        self._controller.send_robot_command(robot_command, self._base_fear_factor)

    @staticmethod
    def _random_base_fear_factor():
        return 1.0 + random.random() / 100.0

    def _send_vis_state(self):

        states_to_send = {}
        for (robot_name, state) in self._states.items():
            states_to_send[robot_name] = (state['x'], state['y'], state['theta'])

        vis_state = VisState(self._robot_name, states_to_send)
        if type(self._target) == type([]) and len(self._target) > 0:
            vis_state.set_target(self._target[0])

        vis_state.set_predictions(self._predictions)

        self._modify_vis_state(vis_state)

        self._variables['base_ff'] = self._base_fear_factor

        for k, v in self._variables.items():
            vis_state.add_variable(k, v)

        self._controller.send_vis_update(vis_state)

    def send_finish_msg(self):
        self._send_state_msg(StateMsgType.finish)

    def send_start_msg(self):
        self._send_state_msg(StateMsgType.start)

    def _send_state_msg(self, type):
        msg = StateMsg(self._robot_name, self._own_robot['timestamp'], self.__class__.__name__, self._base_fear_factor,
                       type)
        msg.set_distance(self._odometer)
        self._controller.send_vis_update(msg)

    def _modify_vis_state(self, vis_state):
        pass

    def _update_states(self):
        new_states = self._controller.request_states()

        if not self._running or len(new_states) == 0:
            return False

        for (robot_name, new_state) in new_states.items():
            org_x = new_state.get_x()
            org_y = new_state.get_y()
            org_theta = new_state.get_theta()
            timestamp = new_state.get_timestamp()
            fear_factor = new_state.get_fear_factor()

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
                         'epsilon': 0.0,
                         'fear_factor': fear_factor}

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

            if robot_name == self._robot_name:
                self._odometer += MeasurementUtils.distance(state['x'], state['y'], x, y)

            state['x'] = x
            state['y'] = y
            state['vx'] = vx
            state['vy'] = vy
            state['v'] = v
            state['theta'] = theta
            state['omega'] = omega
            state['epsilon'] = epsilon
            state['timestamp'] = timestamp
            state['fear_factor'] = fear_factor

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
