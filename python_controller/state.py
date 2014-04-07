__author__ = 'michal'

class State():

    def __init__(self, full_state):
        self._full_state = full_state

    def get_x(self):
        return self._full_state.x

    def get_y(self):
        return self._full_state.y

    def get_theta(self):
        return self._full_state.theta

    def get_timestamp(self):
        return self._full_state.timestamp