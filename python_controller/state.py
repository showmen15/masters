__author__ = 'michal'

class State():

    def __init__(self, full_state):
        self._full_state = full_state

    def get_robot_name(self):
        return self._full_state.robotName

    def get_x(self):
        return self._full_state.x

    def get_y(self):
        return self._full_state.y

    def get_theta(self):
        return self._full_state.theta

    def get_timestamp(self):
        return self._full_state.timestamp

    def __unicode__(self):
        return "%s: x=%f, y=%f, theta=%f, time:%d" \
            % (self._full_state.robotName, self._full_state.x, self._full_state.y,
               self._full_state.theta, self._full_state.timestamp)

    def __str__(self):
        return self.__unicode__()

    def __repr__ (self):
        return self.__unicode__()