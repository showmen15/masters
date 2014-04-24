
class State():
    def __init__(self, robot_name, x, y, theta, timestamp):
        self._robot_name = robot_name
        self._x = x
        self._y = y
        self._theta = theta
        self._timestamp = timestamp

    @staticmethod
    def from_full_state(full_state):
        return State(full_state.robotName, full_state.x, full_state.y,
                      full_state.theta, full_state.timestamp)


    def get_robot_name(self):
        return self._robot_name

    def get_x(self):
        return self._x

    def get_y(self):
        return self._y

    def get_theta(self):
        return self._theta

    def get_timestamp(self):
        return self._timestamp

    def __unicode__(self):
        return "%s: x=%f, y=%f, theta=%f, time:%d" \
               % (self._robot_name, self._x, self._y,
                  self._theta, self._timestamp)

    def __str__(self):
        return self.__unicode__()

    def __repr__ (self):
        return self.__unicode__()

class VisState:

    def __init__(self, robot_name, states_dict):
        self._robot_name = robot_name
        self._states_dict = states_dict

    def get_states_dict(self):
        return self._states_dict

    def get_robot_name(self):
        return self._robot_name