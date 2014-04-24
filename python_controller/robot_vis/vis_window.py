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
            qp.drawText(-20, -10, state.get_robot_name())
            qp.rotate(math.degrees(-state.get_theta()))
            qp.drawRect(-5, -7, 10, 14)
            qp.fillRect(-1, 7, 2, 2, color)

            qp.resetTransform()

    def update_state(self, vis_state):
        assert self._robot_name == vis_state.get_robot_name()
        self._vis_state = vis_state
        self.update()


    def _window_location(self, x, y):
        size = self.size()

        win_x = ((x - self.MIN_X) / self.WIDTH) * size.width()
        win_y = ((y - self.MIN_Y) / self.HEIGHT) * size.height()

        return int(win_x), int(win_y)