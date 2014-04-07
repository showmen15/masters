__author__ = 'michal'

class RobotCommand:

    def __init__(self, front_left, front_right, rear_left, rear_right):
        self._front_left = front_left
        self._front_right = front_right
        self._rear_left = rear_left
        self._rear_right = rear_right

    def get_front_left(self):
        return self._front_left

    def get_front_right(self):
        return self._front_right

    def get_rear_left(self):
        return self._rear_left

    def get_rear_right(self):
        return self._rear_right