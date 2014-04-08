__author__ = 'michal'

import sys, os, struct
import logging

STDIN = 0
STDOUT = 1
PACK = "!h"

class ErlangPort:

    def __init__(self, logger):
        self._logger = logger
        self._logger.info("Erlang port inited")

    def read_msg(self):

        try:
            buf = os.read(STDIN, 2)
            if len(buf) == 2:
                (msg_size,) = struct.unpack(PACK, buf)
                msg = os.read(STDIN, msg_size)

                if self._logger.isEnabledFor('DEBUG'):
                    self._logger.debug("Got message with length: %d" % (len(msg), ))

                if len(msg) == msg_size:
                    return msg

            self.exit("Failed reading message")

        except OSError as e:
            self.exit(e)

    def send_msg(self, msg):
        msg_size = len(msg)
        buf = struct.pack(PACK, msg_size)
        try:
            if os.write(STDOUT, buf) != 2 or os.write(STDOUT, msg) != msg_size:
                self.exit("Failed sending message")

            if self._logger.isEnabledFor('DEBUG'):
                self._logger.debug("Sent %d bytes" % (msg_size, ))

        except OSError as e:
            self.exit(e)

        return True

    def exit(self, cause):
        self._logger.critical("Pipe error. Exiting. " + str(cause))
        sys.exit(1)