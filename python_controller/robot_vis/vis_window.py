#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys, random
from PyQt4 import QtGui, QtCore
import math
from gnuradio.analog.analog_swig import feedforward_agc_cc


class VisWindow(QtGui.QWidget):
    WINDOW_MARGIN = 1.0

    WINDOW_WIDTH = 400
    WINDOW_HEIGHT = 400

    CROSS_HALF_SIZE = 8

    CIRCLE_SIZE = 15

    def __init__(self, robot_name, walls, bounds):
        super(VisWindow, self).__init__()

        self._robot_name = robot_name
        self._walls = walls
        (self._min_x, self._min_y), (self._max_x, self._max_y) = bounds

        self._min_x -= self.WINDOW_MARGIN
        self._max_x += self.WINDOW_MARGIN
        self._min_y -= self.WINDOW_MARGIN
        self._max_y += self.WINDOW_MARGIN

        self._width = abs(self._max_x - self._min_x)
        self._height = abs(self._max_y - self._min_y)

        self._vis_state = None
        self._colors = {}

        self.initUI()

    def initUI(self):

        self.setGeometry(300, 300, self.WINDOW_WIDTH, self.WINDOW_HEIGHT)
        self.setWindowTitle(self._robot_name)
        self.show()

    def paintEvent(self, e):
        qp = QtGui.QPainter()
        qp.begin(self)
        self.redraw(qp)
        qp.end()

    def redraw(self, qp):
        self._draw_walls(qp)
        self._draw_robots(qp)
        self._draw_circles(qp)
        self._draw_target(qp)
        self._draw_info(qp)

    def update_state(self, vis_state):
        assert self._robot_name == vis_state.get_robot_name()
        self._vis_state = vis_state
        self._update_colors()
        self.update()

    def _draw_walls(self, qp):
        for (x1, y1), (x2, y2) in self._walls:
            wx1, wy1 = self._window_location(x1, y1)
            wx2, wy2 = self._window_location(x2, y2)

            qp.setPen(QtCore.Qt.black)
            qp.drawLine(wx1, wy1, wx2, wy2)

    def _update_colors(self):
        for robot_name in self._vis_state.get_state().keys():
            if self._robot_name == robot_name:
                color = QtCore.Qt.black
            else:
                fear_factors = self._vis_state.get_fear_factors()
                if fear_factors is not None:
                    if fear_factors[robot_name] > fear_factors[self._robot_name]:
                        color = QtCore.Qt.red
                    else:
                        color = QtCore.Qt.darkGreen
                else:
                    color = QtCore.Qt.darkBlue

            self._colors[robot_name] = color

    def _draw_robots(self, qp):
        for (robot_name, (x, y, theta)) in self._vis_state.get_state().items():
            color = self._colors[robot_name]
            qp.setPen(color)

            (win_x, win_y) = self._window_location(x, y)
            qp.translate(win_x, win_y)

            #qp.drawText(-30, 25, "(%.2f, %.2f) %.2f" % (x, y, theta))

            fear_factors = self._vis_state.get_fear_factors()
            if fear_factors is not None:
                ff = fear_factors[self._robot_name]
                qp.drawText(-30, 25, "ff: %.2f" % (fear_factors[robot_name]))

            qp.drawText(-20, -15, robot_name)

            qp.rotate(math.degrees(-theta))
            qp.drawRect(-5, -7, 10, 14)
            qp.fillRect(-1, 7, 2, 2, color)

            qp.resetTransform()

    def _draw_info(self, qp):
        qp.setPen(QtCore.Qt.black)

        h_pos = 5
        for (name, value) in self._vis_state.get_variables().items():
            h_pos += 15
            qp.drawText(5, h_pos, "%s: %s" % (name, value))

    def _draw_target(self, qp):
        target = self._vis_state.get_target()
        if target is None:
            return

        (x, y) = target
        (win_x, win_y) = self._window_location(x, y)

        qp.setPen(QtCore.Qt.black)
        qp.drawLine(win_x - self.CROSS_HALF_SIZE, win_y + self.CROSS_HALF_SIZE,
                    win_x + self.CROSS_HALF_SIZE, win_y - self.CROSS_HALF_SIZE)
        qp.drawLine(win_x - self.CROSS_HALF_SIZE, win_y - self.CROSS_HALF_SIZE,
                    win_x + self.CROSS_HALF_SIZE, win_y + self.CROSS_HALF_SIZE)

    def _draw_circles(self, qp):
        circles_dict = self._vis_state.get_predictions()

        if circles_dict is None:
            return

        for (robot_name, circles) in circles_dict.items():
            if circles is None:
                continue

            color = self._colors[robot_name]
            qp.setPen(color)

            for (x, y) in circles:
                (win_x, win_y) = self._window_location(x, y)
                qp.drawEllipse(win_x - self.CIRCLE_SIZE / 2, win_y - self.CIRCLE_SIZE / 2,
                               self.CIRCLE_SIZE, self.CIRCLE_SIZE)


    def _window_location(self, x, y):
        size = self.size()

        win_x = ((x - self._min_x) / self._width) * size.width()
        win_y = ((y - self._min_y) / self._height) * size.height()

        return int(win_x), int(win_y)