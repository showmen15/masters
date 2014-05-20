__author__ = 'michal'

import math


class MeasurementUtils:

    @staticmethod
    def distance(x1, y1, x2, y2):
        a = abs(x1 - x2)
        b = abs(y1 - y2)

        return math.sqrt(a * a + b * b)

    @staticmethod
    def angle(x1, y1, x2, y2):
        x = x2 - x1
        y = y2 - y1

        return math.atan2(x, y)