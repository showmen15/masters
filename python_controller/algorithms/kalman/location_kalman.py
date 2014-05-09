__author__ = 'michal'

from pykalman import KalmanFilter
import numpy

class LocationKalman:

    def __init__(self, start_x, start_y, time_delta):

        position_stdev = 0.05

        A = numpy.asarray([
            [1, 0, time_delta, 0],
            [0, 1, 0, time_delta],
            [0, 0, 1, 0],
            [0, 0, 0, 1]
        ])

        H = numpy.asarray([
            [1, 0, 0, 0],
            [0, 1, 0, 0]
        ])

        r = position_stdev
        r2 = r * r
        R = numpy.asarray([
            [r, 0],
            [0, r]
        ])

        Q = numpy.asarray([
            [r2, 0, 0, 0],
            [0, r2, 0, 0],
            [0, 0, r2, 0],
            [0, 0, 0, r2]
        ])

        initial_state = numpy.asarray([start_x, start_y, 0, 0])

        self._kf = KalmanFilter(transition_matrices=A,
                               observation_matrices=H,
                               transition_covariance=Q,
                               observation_covariance=R,
                               initial_state_mean=initial_state)

        self._means = initial_state
        self._covs = numpy.zeros((4, 4))

    def step(self, x, y):
        (self._means, self._covs) = \
            self._kf.filter_update(self._means, self._covs, observation=numpy.asarray([x, y]))

    def missing_step(self):
        (self._means, self._covs) = \
            self._kf.filter_update(self._means, self._covs, observation=None)


    def get_means(self):
        return tuple(self._means)
