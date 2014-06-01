__author__ = 'michal'

import json
from collections import deque
from measurement_utils import MeasurementUtils

class MazeUtil:

    def __init__(self, roson):
        self._spaces = {}
        self._gate_nodes = {}
        self._space_to_gate_nodes = {}

        self._walls = {}
        self._gates = {}

        self._load_roson(roson)

    def _load_roson(self, roson):

        for space in roson['spaces']:
            space_dict = {

                'walls': [],
                'gates': []
            }
            self._spaces[space['id']] = space_dict

        for wall in roson['walls']:
            x1 = wall['from']['x']
            y1 = wall['from']['y']

            x2 = wall['to']['x']
            y2 = wall['to']['y']

            self._walls[wall['id']] = ((x1, y1), (x2, y2))

        for gate in roson['gates']:
            x1 = gate['from']['x']
            y1 = gate['from']['y']

            x2 = gate['to']['x']
            y2 = gate['to']['y']

            self._gates[gate['id']] = ((x1, y1), (x2, y2))

        for space_wall in roson['space-walls']:
            space = self._spaces[space_wall['spaceId']]
            wall = self._walls[space_wall['wallId']]

            space['walls'].append(wall)

        gate_spaces = set()
        for space_gate in roson['space-gates']:
            gate_spaces.add(space_gate['gateId'])

            space = self._spaces[space_gate['spaceId']]
            gate = self._gates[space_gate['gateId']]

            space['gates'].append(gate)

        self._obtain_bounds()

        for node in roson['nodes']:
            id = node['id']
            x = node['position']['x']
            y = node['position']['y']
            space_id = self.find_space_by_point((x, y))

            if node['kind'] == 'gateNode':
                self._gate_nodes[id] = {
                    'location': (x, y),
                    'space': space_id,
                    'neighbours': []
                }

                if space_id not in self._space_to_gate_nodes:
                    self._space_to_gate_nodes[space_id] = []

                self._space_to_gate_nodes[space_id].append(id)

        for node_node in roson['node-nodes']:
            from_id = node_node['nodeFromId']
            to_id = node_node['nodeToId']

            if from_id in self._gate_nodes and to_id in self._gate_nodes:
                self._gate_nodes[from_id]['neighbours'].append(to_id)

    def _obtain_bounds(self):
        for space in self._spaces.values():
            x_set = set()
            y_set = set()

            for (x1, y1), (x2, y2) in space['walls']:
                x_set.add(x1)
                x_set.add(x2)
                y_set.add(y1)
                y_set.add(y2)

            min_x = min(x_set)
            max_x = max(x_set)
            min_y = min(y_set)
            max_y = max(y_set)

            space['bounds'] = (min_x, min_y), (max_x, max_y)

    def find_space_by_point(self, (x, y)):
        for space_id, space in self._spaces.items():
            (x1, y1), (x2, y2) = space['bounds']
            if x1 <= x <= x2 and y1 <= y <= y2:
                return space_id

        return None

    def find_path(self, (sx, sy), (tx, ty)):
        source_space = self.find_space_by_point((sx, sy))
        target_space = self.find_space_by_point((tx, ty))

        if source_space == target_space:
            return [(tx, ty)]

        to_visit = self._space_to_gate_nodes[source_space][:]
        visited = set()

        parents = {}
        while len(to_visit) > 0:
            node_id = to_visit.pop()
            visited.add(node_id)
            node = self._gate_nodes[node_id]

            if node['space'] == target_space:
                path = [self._gate_nodes[node_id]['location']]
                while node_id in parents:
                    node_id = parents[node_id]
                    path.append(self._gate_nodes[node_id]['location'])

                path.reverse()
                path.append((tx, ty))
                return path

            for nbr_id in node['neighbours']:
                if nbr_id not in visited:
                    to_visit.append(nbr_id)
                    parents[nbr_id] = node_id

        return None

    def find_intersection(self, predictions, radius):
        for x, y in predictions:
            space_id = self.find_space_by_point((x, y))
            if space_id is None:
                return True

            for (wx1, wy1), (wx2, wy2) in self._spaces[space_id]['walls']:
                if wx1 == wx2 and wy1 <= y <= wy2 and abs(x - wx1) < radius:
                    return True

                if wy1 == wy2 and wx1 <= x <= wx2 and abs(y - wy1) < radius:
                    return True

                if MeasurementUtils.distance(x, y, wx1, wy1) < radius:
                    return True

                if MeasurementUtils.distance(x, y, wx2, wy2) < radius:
                    return True

        return False
