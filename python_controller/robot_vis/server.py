__author__ = 'michal'

from PyQt4 import QtGui

import sys
sys.path.append("..")

import cPickle
import json
import time
from datetime import datetime

from robot_model import VisState, StateMsg, StateMsgType

from vis_window import VisWindow
from main_window import MainWindow

import signal
signal.signal(signal.SIGINT, signal.SIG_DFL)

from PyQt4.QtNetwork import QUdpSocket, QHostAddress
from PyQt4.QtCore import QTimer
from pymongo import MongoClient, errors

class RobotVisServer(QUdpSocket):

    HOST = QHostAddress.Any
    PORT = 9010
    REMOTE_PORT = 9015

    MONGO_DB = 'robots'
    MONGO_COLLECTION = 'testcases'

    def __init__(self, roson):
        super(RobotVisServer, self).__init__()

        self._name = roson['name']
        self._timeout = int(roson['timeout'])

        self._windows_dict = {}
        self._walls = []
        self._bounds = None

        self._auto_mode = False
        self._robots_data = None
        self._robots_counter = 0
        self._auto_timer = None
        self._start_time = None
        self._runs_counter = 0

        self._parse_walls(roson)

        try:
            self._mongo_client = MongoClient()
            self._mongo_collection = self._mongo_client[self.MONGO_DB][self.MONGO_COLLECTION]
            print "connected to mongodb"
        except errors.PyMongoError:
            self._mongo_client = None
            self._mongo_collection = None
            print "error connecting to mongodb"

        self.bind(self.HOST, self.PORT)
        self.readyRead.connect(self.read_data)

    def read_data(self):
        while self.hasPendingDatagrams():
            size = self.pendingDatagramSize()
            (str_data, host, port) = self.readDatagram(size)

            msg = cPickle.loads(str_data)

            if isinstance(msg, VisState):
                self.handle_vis_state(msg)
            else:
                self.handle_state_msg(msg)

    def handle_vis_state(self, vis_state):
        robot_name = vis_state.get_robot_name()

        if robot_name in self._windows_dict:
            vis_window = self._windows_dict[robot_name]
        else:
            vis_window = VisWindow(robot_name, self._walls, self._bounds)
            self._windows_dict[robot_name] = vis_window

        vis_window.update_state(vis_state)

    def enable_auto_mode(self):
        self._auto_mode = True
        self.reset_auto_mode()

    def disable_auto_mode(self):
        self._auto_mode = False

    def reset_auto_mode(self):
        self._robots_data = {}
        self._robots_counter = 0

        self._send_message("stop")
        time.sleep(1)
        self._send_message("reset")
        time.sleep(1)
        self._send_message("start")

        self._start_time = datetime.now()
        self._runs_counter += 1

        if self._auto_timer is not None:
            self._auto_timer.stop()
            self._auto_timer.deleteLater()

        self._auto_timer = QTimer()
        self._auto_timer.timeout.connect(self._auto_timeout)
        self._auto_timer.setSingleShot(True)
        self._auto_timer.start(1000 * self._timeout)

        print "starting run: %i" % (self._runs_counter, )

    def _auto_timeout(self):
        self._finish_auto_run()

    def _finish_auto_run(self):
        print "finished run: %i" % (self._runs_counter, )

        finish_time = datetime.now()
        total_time = (finish_time - self._start_time).total_seconds()

        result = {
            'name': self._name,
            'start_time': self._start_time,
            'finish_time': finish_time,
            'total_time': total_time,
            'robots': {}
        }

        try:

            for robot_name, robot_data in self._robots_data.items():

                if 'finish_time' in robot_data:
                    finish_time = robot_data['finish_time']
                    total_time = finish_time - robot_data['start_time']
                else:
                    finish_time = None
                    total_time = None

                distance = 'distance' in robot_data and robot_data['distance'] or None

                result['robots'][robot_name] = {
                    'algorithm': robot_data['algorithm'],
                    'start_time': robot_data['start_time'],
                    'finish_time': finish_time,
                    'total_time': total_time,
                    'distance': distance
                }

        except KeyError:
            print "testcase failed"

        if self._mongo_collection is not None:
            try:
                id = self._mongo_collection.insert(result)
                print "added result to mongodb with id %s" % (str(id), )
            except errors.PyMongoError:
                print "failed to add result to mongodb"



        self.reset_auto_mode()


    def handle_state_msg(self, state_msg):
        if not self._auto_mode:
            return

        robot_name = state_msg.get_robot_name()
        if robot_name not in self._robots_data:
            self._robots_data[robot_name] = {}

        if state_msg.get_type() == StateMsgType.start:
            self._robots_data[robot_name]['start_time'] = state_msg.get_timestamp()
            self._robots_data[robot_name]['algorithm'] = state_msg.get_algorithm()
            self._robots_counter += 1
        elif state_msg.get_type() == StateMsgType.finish:
            self._robots_data[robot_name]['finish_time'] = state_msg.get_timestamp()
            self._robots_data[robot_name]['distance'] = state_msg.get_distance()
            self._robots_counter -= 1

        if self._robots_counter == 0:
            self._finish_auto_run()


    def start(self):
        self._send_message("start")

    def stop(self):
        self.disable_auto_mode()
        self._send_message("stop")

    def reset(self):
        self._send_message("reset")

    def _send_message(self, message):
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