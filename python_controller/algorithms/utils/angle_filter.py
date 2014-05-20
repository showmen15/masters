__author__ = 'michal'

from collections import deque
import numpy as np
import math

class AngleFilter:


    def __init__(self, size, time_delta):
        self._size = size
        self._time_delta = time_delta
        self._deque = deque(maxlen=size)

    def push(self, angle):
        self._deque.append(angle)

    def missing_step(self):
        pass

        #if len(self._deque) < 2:
        #    return
        #
        #self.push(2 * self._deque[-1] - self._deque[-2])


    def get_ang_speed(self):
        if len(self._deque) < 2:
            return None

        speeds = []

        prev = None
        for act in self._deque:
            if prev is None:
                prev = act
                continue

            diff = act-prev

            if abs(diff) > math.pi:
                diff = math.copysign(2 * math.pi - abs(diff), prev)

            ang_speed = diff / float(self._time_delta)
            speeds.append(ang_speed)

            prev = act

        return round(np.mean(speeds), 2)

    def clear(self):
        self._deque.clear()