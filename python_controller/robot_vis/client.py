__author__ = 'michal'

from PyQt4.QtNetwork import QUdpSocket, QHostAddress

import cPickle
import logging

class RobotVisClient(QUdpSocket):

    def __init__(self, hostname, port):
        super(RobotVisClient, self).__init__()

        self._hostname = QHostAddress(hostname)
        self._port = port

        self._logger = logging.getLogger("vis_client")

    def send_update(self, vis_update):
        if self._logger.isEnabledFor('DEBUG'):
            self._logger.debug("Sending VisState message, host: %s, port: %d" % (self._hostname.toString(), self._port))

        try:
            str_data = cPickle.dumps(vis_update)

            if self.writeDatagram(str_data, self._hostname, self._port) < 0:
                self._logger.error("Error sending VisState message: %s" % (self.errorString(), ))

        except cPickle.PickleError as e:
            self._logger.error("PickleError: %s" % (e, ))


def main():
    client = RobotVisClient("sm", 9010)
    data = cPickle.dumps([1,2,3])
    client.send_update(data)

if __name__ == '__main__':
    main()
