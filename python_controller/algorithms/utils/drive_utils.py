__author__ = 'michal'

import time
from robot_command import RobotCommand

class DriveUtils:

    @staticmethod
    def go_and_rotate(controller):

        l_speed = 0.5
        r_speed = 0.5

        controller.send_robot_command(RobotCommand(l_speed, r_speed, l_speed, r_speed))

        time.sleep(4)


        l_speed = 0.5
        r_speed = 0.2

        controller.send_robot_command(RobotCommand(l_speed, r_speed, l_speed, r_speed))

        while True:
            time.sleep(1)


