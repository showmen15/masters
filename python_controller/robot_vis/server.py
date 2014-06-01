__author__ = 'michal'

from PyQt4 import QtGui

import sys
sys.path.append("..")

import cPickle
import json

import robot_model

from vis_window import VisWindow
from main_window import MainWindow

import signal
signal.signal(signal.SIGINT, signal.SIG_DFL)

from PyQt4.QtNetwork import QUdpSocket, QHostAddress

class RobotVisServer(QUdpSocket):

    HOST = QHostAddress.Any
    PORT = 9010
    REMOTE_PORT = 9015

    def __init__(self, roson):
        super(RobotVisServer, self).__init__()

        self._windows_dict = {}
        self._walls = []
        self._bounds = None

        self._parse_walls(roson)

        self.bind(self.HOST, self.PORT)
        self.readyRead.connect(self.read_data)

    def read_data(self):
        while self.hasPendingDatagrams():
            size = self.pendingDatagramSize()
            (str_data, host, port) = self.readDatagram(size)

            vis_state = cPickle.loads(str_data)
            self.handle_vis_state(vis_state)

    def handle_vis_state(self, vis_state):
        robot_name = vis_state.get_robot_name()

        if robot_name in self._windows_dict:
            vis_window = self._windows_dict[robot_name]
        else:
            vis_window = VisWindow(robot_name, self._walls, self._bounds)
            self._windows_dict[robot_name] = vis_window

        vis_window.update_state(vis_state)

    def send_message(self, message):
        self.writeDatagram(message, QHostAddress.LocalHost, self.REMOTE_PORT)

    def _parse_walls(self, roson):
        assert len(roson['walls']) > 0

        xs = set()
        ys = set()

        for wall in roson['walls']:
            x1 = wall['from']['x']
            y1 = wall['from']['y']
            x2 = wall['to']['x']
            y2 = wall['to']['y']

            xs.add(x1)
            xs.add(x2)
            ys.add(y1)
            ys.add(y2)

            self._walls.append(((x1, y1), (x2, y2)))

        self._bounds = (min(xs), min(ys)), (max(xs), max(ys))

def main():
    if len(sys.argv) != 2:
        print "Specify roson file"
        sys.exit(1)

    json_file = open(sys.argv[1])
    roson = json.load(json_file)

    app = QtGui.QApplication(sys.argv)

    server = RobotVisServer(roson)
    main_window = MainWindow(server)

    sys.exit(app.exec_())

if __name__ == '__main__':
    main()