__author__ = 'michal'

from pykalman import KalmanFilter
import numpy
import math


class AngleKalman:

    def __init__(self, start_angle, time_delta):
        angle_stdev = 0.05

        A = numpy.asarray([
            [1, time_delta, 0.5 * time_delta * time_delta],
            [0, 1, time_delta],
            [0, 0, 1]
        ])

        H = numpy.asarray([
            [1, 0, 0]
        ])

        r = angle_stdev
        r2 = r * r
        R = numpy.asarray([
            [r],
        ])

        Q = numpy.asarray([
            [r2, 0, 0],
            [0, r2, 0],
            [0, 0, r2]
        ])

        initial_state = numpy.asarray([start_angle, 0, 0])

        self._kf = KalmanFilter(transition_matrices=A,
                                observation_matrices=H,
                                transition_covariance=Q,
                                observation_covariance=R,
                                initial_state_mean=initial_state)

        self._means = initial_state
        self._covs = numpy.zeros((3, 3))
        self._last_a = start_angle

    def step(self, a):
        diff = abs(a - self._last_a)
        if diff > math.pi:
            corr = math.copysign(2 * math.pi, a)
            self._means[0] += corr
            self._last_a += corr

        (self._means, self._covs) = \
            self._kf.filter_update(self._means, self._covs, observation=numpy.asarray([a]))

        self._last_a = a

    def missing_step(self):
        (self._means, self._covs) = \
            self._kf.filter_update(self._means, self._covs, observation=None)

    def get_means(self):
        (a, o, e) = self._means
        new_a = (a + math.pi) % (2 * math.pi) - math.pi
        return new_a, o, e

