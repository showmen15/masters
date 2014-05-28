__author__ = 'michal'

import math
from robot_model import RobotConstants


class PredictionUtils:

    CIRCLES_DELTA = 0.5
    CIRCLES_NUM = 10

    @staticmethod
    def predict_positions_with_fun(x, y, theta, target, fun):
        l = RobotConstants.ROBOT_WIDTH
        circles = []

        for i in range(1, PredictionUtils.CIRCLES_NUM):
            v, omega = fun(x, y, theta, target)

            if abs(omega) < 0.001:
                z = v * PredictionUtils.CIRCLES_DELTA

                dx = z * math.sin(theta)
                dy = z * math.cos(theta)

                x += dx
                y += dy

            else:
                R = v / omega

                ICCx = x + R * math.cos(theta)
                ICCy = y - R * math.sin(theta)

                odt = omega * PredictionUtils.CIRCLES_DELTA
                cos_odt = math.cos(odt)
                sin_odt = math.sin(odt)

                x = (y - ICCy) * sin_odt + (x - ICCx) * cos_odt + ICCx
                y = (y - ICCy) * cos_odt - (x - ICCx) * sin_odt + ICCy
                theta += odt

            circles.append((x, y))

        return circles

    @staticmethod
    def predict_positions(x, y, v, theta, omega):

        #print "predict, x: %f, y: %f, v: %f, theta: %f, omega: %f" % (x, y, v, theta, omega)

        l = RobotConstants.ROBOT_WIDTH

        if abs(omega) < 0.001:
            return PredictionUtils._inline_helper(x, y, v, theta)

        R = v / omega

        return PredictionUtils._arc_helper(x, y, R, theta, omega)

    @staticmethod
    def _arc_helper(x, y, R, theta, omega):
        circles = []

        ICCx = x + R * math.cos(theta)
        ICCy = y - R * math.sin(theta)

        circles.append((x, y))
        for i in range(1, PredictionUtils.CIRCLES_NUM):
            odt = omega * PredictionUtils.CIRCLES_DELTA * i
            cos_odt = math.cos(odt)
            sin_odt = math.sin(odt)

            x2 = (y - ICCy) * sin_odt + (x - ICCx) * cos_odt + ICCx
            y2 = (y - ICCy) * cos_odt - (x - ICCx) * sin_odt + ICCy
            circles.append((x2, y2))

        return circles

    @staticmethod
    def _inline_helper(x, y, speed, angle):
        z = speed * PredictionUtils.CIRCLES_DELTA

        dx = z * math.sin(angle)
        dy = z * math.cos(angle)

        circles = [(x + dx * i, y + dy * i) for i in range(PredictionUtils.CIRCLES_NUM)]
        return circles

