__author__ = 'michal'

import sys, os, struct
import logging

STDIN = 0
STDOUT = 1
PACK = "!h"

class ErlangPort:

    def __init__(self):
        self._logger = logging.getLogger('controller.erl_port')
        self._logger.info("Erlang port inited")

    def read_msg(self):
        buf = os.read(STDIN, 2)
        if len(buf) == 2:
            (msg_size,) = struct.unpack(PACK, buf)
            msg = os.read(STDIN, msg_size)

            if self._logger.isEnabledFor('DEBUG'):
                self._logger.debug("Got message with length: %d" % (msg_size, ))
            return msg


    def send_msg(self, msg):
        msg_size = len(msg)
        buf = struct.pack(PACK, msg_size)
        if os.write(STDOUT, buf) != 2 or os.write(STDOUT, msg) != msg_size:
            self._logger.error("Failed sending message")
            return False

        if self._logger.isEnabledFor('DEBUG'):
            self._logger.debug("Sent %d bytes" % (msg_size, ))

        return True