-record(wheels_cmd, {
	front_left, 
	front_right, 
	rear_left,
	rear_right
}).

-record(robot_state, {
	x,
	y,
	theta,
	timestamp
}).