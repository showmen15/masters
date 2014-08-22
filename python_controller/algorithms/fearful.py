__author__ = 'michal'

import random
import time
import math
from robot_command import RobotCommand
from robot_model import RobotConstants
from utils.prediction_utils import PredictionUtils
from algorithms.abstract_algorithm import AbstractAlgorithm
from utils.measurement_utils import MeasurementUtils
from utils.maze_util import MazeUtil
from operator import itemgetter

class FearfulAlgorithm(AbstractAlgorithm):
    CRUISE_SPEED = 0.5
    ANGLE_MEASURE_STEPS = 5
    CIRCLES_RADIUS = 0.5
    VARIANTS_MAX_TIME = 0.015
    FF_RADIUS = 2.0
    YIELD_DELAY = 3 * 1000 * 1000
    RUNAWAY_TIME = 10000.0 * 1000 * 1000

    def __init__(self, controller, robot_name, roson):
        super(FearfulAlgorithm, self).__init__(controller, robot_name)
        self._logger.info("Simple algorithm started")

        self._INITIAL_FEAR_FACTOR = 1.0 + int(robot_name[5:])/100.0

        self._target = None
        self._target_reached = False
        self._last_params = None
        self._maze_util = MazeUtil(roson)
        self._space_id = None

        self._fear_factors = None
        self._yield_timestamps = {}
        self._yield_set = set()

        self._own_fear_factor = self._INITIAL_FEAR_FACTOR

    def reset(self):
        super(FearfulAlgorithm, self).reset()

        self._target = None
        self._target_reached = False
        self._last_params = None
        self._space_id = None

        self._fear_factors = None
        self._yield_timestamps = {}
        self._yield_set = set()

        self._own_fear_factor = self._INITIAL_FEAR_FACTOR

    def _loop(self, avail_time):
        self._compute_fear_factors()
        self._navigate(avail_time)

    def _compute_fear_factors(self):
        self._fear_factors = {}

        for robot_name, state in self._states.items():
            x = state['x']
            y = state['y']
            theta = state['theta']
            fear_factor = state['fear_factor']

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

                fear_factor += (1 - dist / FearfulAlgorithm.FF_RADIUS) * math.cos(angle_diff) \
                    * self._states[robot_name]['fear_factor']

            self._fear_factors[robot_name] = fear_factor

    def _modify_vis_state(self, vis_state):
        vis_state.set_fear_factors(self._fear_factors)

    def _navigate(self, avail_time):
        dist = None

        x = self._own_robot['x']
        y = self._own_robot['y']
        theta = self._own_robot['theta']
        v = FearfulAlgorithm.CRUISE_SPEED

        if self._target is None:
            tx, ty = self._controller.get_new_target()
            self._target = self._maze_util.find_path((x, y), (tx, ty))

        if not self._target_reached and self._target_distance() < 0.1:

            if len(self._target) > 1:
                self._target.pop(0)
            else:
                new_target = self._controller.get_new_target()
                if new_target is None or new_target != self._target[0]:
                    tx, ty = new_target
                    self._target = self._maze_util.find_path((x, y), (tx, ty))
                else:
                    self._target_reached = True
                    self._logger.info("Target reached")
                    self.send_finish_msg()

        space_id = self._maze_util.find_space_by_point((x, y))
        if space_id != self._space_id:
            if self._space_id is not None:
                tx, ty = self._target[-1]
                self._target = self._maze_util.find_path((x, y), (tx, ty))

            self._space_id = space_id


        if self._avoid_close_objects(x, y, theta):
            return

        target = self._target[0]
        tx, ty = target
        bearing = MeasurementUtils.angle(x, y, tx, ty)

        preds = PredictionUtils.predict_positions(x, y, v, bearing, 0.0)
        preds = FearfulAlgorithm._cut_predictions(preds, target)

        self._predictions[self._robot_name] = preds
        self._variables['rate'] = self._rate_predictions(preds)

        target_dist = MeasurementUtils.distance(x, y, tx, ty)

        if not self._find_intersections(preds, self.CIRCLES_RADIUS * 1.5, 0):
            self._own_fear_factor = self._INITIAL_FEAR_FACTOR

            v, omega = FearfulAlgorithm._navigate_fun(v, theta, bearing, False)

            if target_dist < 0.2:
                v *= target_dist

            if target_dist < 0.05:
                v = 0

            Vl, Vr = FearfulAlgorithm._get_wheel_speeds(v, omega)

            self._send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

            return

        for robot_name in list(self._yield_set):
            rx = self._states[robot_name]['x']
            ry = self._states[robot_name]['y']
            if MeasurementUtils.distance(x, y, rx, ry) > 2.0:
                self._yield_set.remove(robot_name)

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

        try_stop = True

        variants_stop_time = time.time() + avail_time - 0.001
        var_num = 0
        while time.time() < variants_stop_time:
            var_num += 1

            if try_stop:
                rand_v = 0.0
                rand_bearing = theta
                rand_radius = 0.0
                try_stop = False
            else:
                rand_v = random.uniform(0.0, self.CRUISE_SPEED * 2.0)
                rand_bearing = random.uniform(-math.pi, math.pi)
                rand_radius = random.uniform(0.0, self.CRUISE_SPEED * PredictionUtils.CIRCLES_DELTA * PredictionUtils.CIRCLES_NUM)

            mp_x = rand_radius * math.cos(rand_bearing) + x
            mp_y = rand_radius * math.sin(rand_bearing) + y
            rand_midpoint = mp_x, mp_y

            rand_preds = PredictionUtils.predict_positions(x, y, rand_v, rand_bearing, 0.0)
            rand_preds = FearfulAlgorithm._cut_predictions(rand_preds, rand_midpoint)
            rand_rate = self._rate_predictions(rand_preds)

            if self._find_intersections(rand_preds, self.CIRCLES_RADIUS * 1.5, 1):
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

            self._send_robot_command(
                RobotCommand(Vl, Vr, Vl, Vr))

    def _avoid_close_objects(self, x, y, theta):
        distances = self._distances_to_robots()

        new_yield_timestamps = {}

        if self._own_robot['v'] <= 0.1:
            for robot_name, distance in distances:
                if distance > FearfulAlgorithm.CIRCLES_RADIUS * 2.0:
                    break

                if self._fear_factors[self._robot_name] > self._fear_factors[robot_name]:
                    if robot_name not in self._yield_timestamps:
                        new_yield_timestamps[robot_name] = self._own_robot['timestamp'] + self.YIELD_DELAY
                    else:
                        timestamp = self._yield_timestamps[robot_name]
                        if self._own_robot['timestamp'] > timestamp:
                            self._yield_set.add(robot_name)
                            self._own_fear_factor = self._states[robot_name]['fear_factor'] - 0.1
                        else:
                            new_yield_timestamps[robot_name] = timestamp

        self._yield_timestamps = new_yield_timestamps

        bearings = set()

        for robot_name, distance in distances:
            if distance > FearfulAlgorithm.CIRCLES_RADIUS * 1:
                break

            if self._fear_factors[self._robot_name] < self._fear_factors[robot_name]:

                state = self._states[robot_name]
                rx = state['x']
                ry = state['y']

                bearing_to_robot = MeasurementUtils.angle(x, y, rx, ry)
                #bearings.add(bearing_to_robot)
            else:
                self._send_stop_command()
                return True

        for wx, wy in self._maze_util.get_close_walls_points(x, y, 0.25):
            bearing_to_wall = MeasurementUtils.angle(x, y, wx, wy)
            bearings.add(bearing_to_wall)


        if len(bearings) == 0:
            return False
        else:
            bearings = list(bearings)
            bearings.sort()
            bearings_len = len(bearings)
            bearings.append(bearings[0] + 2.0 * math.pi)

            max_gap = 0
            best_bearing = None
            for i in range(bearings_len):
                gap = abs(bearings[i+1] - bearings[i])
                if gap > max_gap:
                    max_gap = gap
                    best_bearing = MeasurementUtils.normalize_angle(bearings[i] + 0.5 * (bearings[i+1] - bearings[i]))

            v, omega = FearfulAlgorithm._navigate_fun(FearfulAlgorithm.CRUISE_SPEED * 0.5, theta, best_bearing, True)

            preds = PredictionUtils.predict_positions(x, y, v, best_bearing, 0.0)
            self._predictions[self._robot_name] = preds

        Vl, Vr = FearfulAlgorithm._get_wheel_speeds(v, omega)

        self._send_robot_command(
            RobotCommand(Vl, Vr, Vl, Vr))

        return True

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

        if abs(omega) > 0.5:
            v = 0.0

        return v, omega

    @staticmethod
    def _get_wheel_speeds(v, omega):
        Vr = v + 0.5 * RobotConstants.ROBOT_WIDTH * omega
        Vl = 2 * v - Vr

        return Vl, Vr

    def _target_distance(self):
        assert type(self._target) == type([])
        assert len(self._target) > 0

        (tx, ty) = self._target[0]
        mx = self._own_robot['x']
        my = self._own_robot['y']

        return MeasurementUtils.distance(tx, ty, mx, my)

    def _target_angle(self):
        assert type(self._target) == type([])
        assert len(self._target) > 0

        (tx, ty) = self._target[0]
        mx = self._own_robot['x']
        my = self._own_robot['y']

        return MeasurementUtils.angle(mx, my, tx, ty)

    @staticmethod
    def _cut_predictions(predictions, target):
        (tx, ty) = target

        for i in range(len(predictions)):
            (px, py) = predictions[i]
            if MeasurementUtils.distance(px, py, tx, ty) < 0.2:
                return predictions[:i+1] + [(px, py)] * (len(predictions) - i - 1)

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

        (px, py) = predictions[-1]

        if len(self._yield_set) > 0:
            robot_name = list(self._yield_set)[0]
            rx = self._states[robot_name]['x']
            ry = self._states[robot_name]['y']

            return 1.0 - MeasurementUtils.distance(px, py, rx, ry) / (self.CRUISE_SPEED * PredictionUtils.CIRCLES_DELTA * PredictionUtils.CIRCLES_NUM * 0.5)
        else:
            tx, ty = self._target[0]
            return MeasurementUtils.distance(px, py, tx, ty)

    def _find_intersections(self, own_predictions, radius, ignored_num):
        assert len(own_predictions) > 0

        for (robot_name, other_predictions) in self._predictions.items():
            if self._robot_name == robot_name:
                continue

            if self._fear_factors[self._robot_name] > self._fear_factors[robot_name]:
                continue

            for ((mx, my), (ox, oy)) in zip(own_predictions[ignored_num:], other_predictions[ignored_num:]):
                dst = MeasurementUtils.distance(mx, my, ox, oy)

                if dst <= radius:
                    return True

        maze = self._maze_util.find_intersection(own_predictions, 0.25)

        return maze