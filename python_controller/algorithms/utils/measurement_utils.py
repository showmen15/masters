__author__ = 'michal'

import math


class MeasurementUtils:

    @staticmethod
    def distance(x1, y1, x2, y2):
        a = x1 - x2
        b = y1 - y2

        return math.sqrt(a * a + b * b)

    @staticmethod
    def angle(x1, y1, x2, y2):
        x = x2 - x1
        y = y2 - y1

        return math.atan2(x, y)

    @staticmethod
    def normalize_angle(angle):
        while abs(angle) > math.pi:
            angle -= math.copysign(2.0 * math.pi, angle)

        return angle

    @staticmethod
    def normalize_min_max(x,min,max):
        new_min = -1
        new_max = 1

        result = (((x - min) * (new_max - new_min))/ (max - min)) + new_min
        return result