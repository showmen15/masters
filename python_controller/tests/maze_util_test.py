__author__ = 'michal'

import json
from algorithms.utils.maze_util import MazeUtil

if __name__ == "__main__":
    f = open('in.roson')
    roson = json.load(f)

    maze_util = MazeUtil(roson)

    print maze_util._spaces
    print maze_util._gate_nodes
    print maze_util._space_to_gate_nodes

    path = [(5.0, 5.1005), (4.999730473151788, 4.899578864299669), (6.699513291415833, 1.7001145196668628), (6.900447213595499, 1.70022360679775), (8.3, 3.0)]
    assert maze_util.find_path((5.0, 8.0), (8.3, 3.0)) == path

    assert maze_util.find_intersection([(0.6, 0.4)], 0.9) is True
    assert maze_util.find_intersection([(1.2, 1.1)], 1.0) is False
    assert maze_util.find_intersection([(-5.0, -5.0)], 0.0) is True
    assert maze_util.find_intersection([(5.0, 5.0)], 0.3) is False