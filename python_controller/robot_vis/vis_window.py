#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys, random
from PyQt4 import QtGui, QtCore
import math

class VisWindow(QtGui.QWidget):

    MAX_X = 6
    MIN_X = -MAX_X

    MAX_Y = 6
    MIN_Y = -MAX_Y

    WIDTH = abs(MAX_X - MIN_X)
    HEIGHT = abs(MAX_Y - MIN_Y)

    WINDOW_WIDTH = 400
    WINDOW_HEIGHT = 400

    CROSS_HALF_SIZE = 8

    CIRCLE_SIZE = 15

    def __init__(self, robot_name):
        super(VisWindow, self).__init__()

        self._robot_name = robot_name
        self._vis_state = None

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
        self._draw_robots(qp)
        self._draw_circles(qp)
        self._draw_target(qp)
        self._draw_info(qp)

    def update_state(self, vis_state):
        assert self._robot_name == vis_state.get_robot_name()
        self._vis_state = vis_state
        self.update()

    def _draw_robots(self, qp):
        for state in self._vis_state.get_states_dict().values():
            if self._robot_name == state.get_robot_name():
                color = QtCore.Qt.red
            else:
                color = QtCore.Qt.black

            qp.setPen(color)

            x = state.get_x()
            y = state.get_y()
            (win_x, win_y) = self._window_location(x, y)
            qp.translate(win_x, win_y)

            qp.drawText(-30, 25, "(%.2f, %.2f) %.2f" % (state.get_x(), state.get_y(), state.get_theta()))
            qp.drawText(-20, -15, state.get_robot_name())

            qp.rotate(math.degrees(-state.get_theta()))
            qp.drawRect(-5, -7, 10, 14)
            qp.fillRect(-1, 7, 2, 2, color)

            qp.resetTransform()

    def _draw_info(self, qp):
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

        qp.setPen(QtCore.Qt.red)
        qp.drawLine(win_x - self.CROSS_HALF_SIZE, win_y + self.CROSS_HALF_SIZE,
                    win_x + self.CROSS_HALF_SIZE, win_y - self.CROSS_HALF_SIZE)
        qp.drawLine(win_x - self.CROSS_HALF_SIZE, win_y - self.CROSS_HALF_SIZE,
                    win_x + self.CROSS_HALF_SIZE, win_y + self.CROSS_HALF_SIZE)

    def _draw_circles(self, qp):
        circles_dict = self._vis_state.get_circles_dict()

        if circles_dict is None:
            return

        for (robot_name, circles) in circles_dict.items():
            if self._robot_name == robot_name:
                color = QtCore.Qt.red
            else:
                color = QtCore.Qt.black

            qp.setPen(color)

            for (x, y) in circles:
                (win_x, win_y) = self._window_location(x, y)
                qp.drawEllipse(win_x - self.CIRCLE_SIZE / 2, win_y - self.CIRCLE_SIZE / 2,
                               self.CIRCLE_SIZE, self.CIRCLE_SIZE)


    def _window_location(self, x, y):
        size = self.size()

        win_x = ((x - self.MIN_X) / self.WIDTH) * size.width()
        win_y = ((y - self.MIN_Y) / self.HEIGHT) * size.height()

        return int(win_x), int(win_y)