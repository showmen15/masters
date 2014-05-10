__author__ = 'michal'

import time

class TimeUtil:

    def __init__(self, interval, measure_steps, logger):
        self._logger = logger
        self._interval = interval
        self._measure_steps = measure_steps

        self._step_times = []
        self._period_start = None
        self._step_start = None
        self._avg_start = None
        self._compute_time_sum = 0

        self._counter = 0

    def start_step(self):
        act_time = time.time()
        self._step_start = act_time

        if self._counter == 0:
            self._avg_start = act_time

        self._counter += 1

    def start_sleep(self):
        compute_time = time.time() - self._step_start
        self._compute_time_sum += compute_time

        time_diff = self._interval - compute_time
        if time_diff > 0:
            time.sleep(time_diff)

        act_time = time.time()

        if self._counter == self._measure_steps:
            avg_period = (act_time - self._avg_start) / float(self._measure_steps) * 1000.0
            avg_compute_time = self._compute_time_sum / float(self._measure_steps) * 1000.0

            self._logger.info("avg period: %f ms" % avg_period)
            self._logger.info("avg compute time: %f ms" % avg_compute_time)

            self._compute_time_sum = 0
            self._counter = 0