__author__ = 'michal'

import json
from collections import defaultdict
from heapq import *
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
            kind = node['kind']
            space_id = self.find_space_by_point((x, y))

            if kind == 'gateNode':
                self._gate_nodes[id] = {
                    'location': (x, y),
                    'space': space_id,
                    'neighbours': set()
                }

                if space_id not in self._space_to_gate_nodes:
                    self._space_to_gate_nodes[space_id] = []

                self._space_to_gate_nodes[space_id].append(id)

        node_nodes = {}
        for node_node in roson['node-nodes']:
            from_id = node_node['nodeFromId']
            to_id = node_node['nodeToId']

            if from_id not in node_nodes:
                node_nodes[from_id] = []

            node_nodes[from_id].append(to_id)

        for gate_node_id, gate_node in self._gate_nodes.items():
            x1, y1 = gate_node['location']

            for nbr_id in node_nodes[gate_node_id]:
                if nbr_id in self._gate_nodes:
                    x2, y2 = self._gate_nodes[nbr_id]['location']
                    dist = MeasurementUtils.distance(x1, y1, x2, y2)

                    self._gate_nodes[gate_node_id]['neighbours'].add((nbr_id, dist))
                else:
                    for space_node_nbr_id in node_nodes[nbr_id]:
                        if space_node_nbr_id != gate_node_id:
                            x2, y2 = self._gate_nodes[space_node_nbr_id]['location']
                            dist = MeasurementUtils.distance(x1, y1, x2, y2)

                            self._gate_nodes[gate_node_id]['neighbours'].add((space_node_nbr_id, dist))


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

        g = defaultdict(list)

        for nbr_id in self._space_to_gate_nodes[source_space][:]:
            nbr_x, nbr_y = self._gate_nodes[nbr_id]['location']
            nbr_dist = MeasurementUtils.distance(sx, sy, nbr_x, nbr_y)
            g['s'].append((nbr_dist, nbr_id))

        for nbr_id in self._space_to_gate_nodes[target_space][:]:
            nbr_x, nbr_y = self._gate_nodes[nbr_id]['location']
            nbr_dist = MeasurementUtils.distance(tx, ty, nbr_x, nbr_y)
            g[nbr_id].append((nbr_dist, 't'))

        for gate_id, gate in self._gate_nodes.items():
            for (nbr_id, nbr_dist) in gate['neighbours']:
                g[gate_id].append((nbr_dist, nbr_id))

        q, seen = [(0, 's', [])], set()
        while q:
            cost, v1, path = heappop(q)
            if v1 not in seen:
                seen.add(v1)

                path = path[:]
                path.append(v1)
                if v1 == 't':
                    targets = map(lambda n: self._gate_nodes[n]['location'], path[1:-1])
                    targets.append((tx, ty))

                    return targets

                for c, v2 in g.get(v1, ()):
                    if v2 not in seen:
                        heappush(q, (cost + c, v2, path))

        return None

    def find_intersection(self, predictions, radius):
        for x, y in predictions:
            space_id = self.find_space_by_point((x, y))
            if space_id is None:
                return True

            for (wx1, wy1), (wx2, wy2) in self._spaces[space_id]['walls']:
                if wx1 == wx2 and min(wy1, wy2) < y < max(wy1, wy2) and abs(wx1 - x) < radius:
                    return True

                if wy1 == wy2 and min(wx1, wx2) < x < max(wx1, wx2) and abs(wy1 - y) < radius:
                    return True

                if MeasurementUtils.distance(x, y, wx1, wy1) < radius:
                    return True

                if MeasurementUtils.distance(x, y, wx2, wy2) < radius:
                    return True

        return False

    def get_close_walls_points(self, x, y, radius):
        space_id = self.find_space_by_point((x, y))
        space_walls = self._spaces[space_id]['walls']

        points = set()
        for (wx1, wy1), (wx2, wy2) in space_walls:
            if wx1 == wx2 and min(wy1, wy2) < y < max(wy1, wy2) and abs(wx1 - x) < radius:
                points.add((wx1, y))

            if wy1 == wy2 and min(wx1, wx2) < x < max(wx1, wx2) and abs(wy1 - y) < radius:
                points.add((x, wy1))

            if MeasurementUtils.distance(x, y, wx1, wy1) < radius:
                points.add((wx1, wy1))

            if MeasurementUtils.distance(x, y, wx2, wy2) < radius:
                points.add((wx2, wy2))

        return points