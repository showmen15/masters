# coding=utf8
__author__ = 'michal'

from pymongo import MongoClient, errors
import numpy as np
import sys

import matplotlib.pyplot as plt

MONGO_DB = 'robots'
MONGO_COLLECTION = 'testcases'

TESTCASE = sys.argv[1]
MAX_TIME = 120
MAX_DIST = 100

def print_stats(name, x):
    try:
        mean = np.mean(x)
        std = np.std(x)
        median = np.median(x)
        x_min = min(x)
        x_max = max(x)
        print "%s, mean: %.2f, std: %.2f, median: %.2f, min: %.2f, max: %.2f" % (name, mean, std, median, x_min, x_max)
    except:
        pass

def compute(client, algorithm, ax, ax_dist, ax_scatter, title):
    print algorithm

    success_count = collection.find(
        {'name': TESTCASE,
         'robots.algorithm': algorithm,
        'robots.total_time' : {'$ne' : None}}).count()

    fail_count = collection.find({
        'name': TESTCASE,
        'robots.algorithm': algorithm,
        'robots.total_time' : None}).count()

    sum = success_count + fail_count
    if sum != 0:
        ratio = float(success_count) / sum
        print "success_count: %i, fail_count: %i, ratio: %f" % (success_count, fail_count, ratio)

    res = collection.find({
        'name': TESTCASE,
        'robots.algorithm': algorithm,
        })

    try:
        for tc in res:
            robots = sorted(tc['robots'], key=lambda r: r['base_ff'], reverse=True)
            robots = map(lambda r: r['robot_name'], robots)

            #print not tc['timeouted'], robots
    except KeyError:
        pass

    res = collection.find({
        'name': TESTCASE,
        'robots.algorithm': algorithm,
        'robots.total_time' : {'$ne' : None}
        })

    times = {}
    x_dist = []
    x = []

    for tc in res:
        max_time = 0.0
        dist_sum = 0.0

        for r in tc['robots']:
            robot_name = r['robot_name']
            total_time = float(r['total_time']) / 1000000

            if robot_name not in times:
                times[robot_name] = []

            times[robot_name].append(total_time)
            if total_time > max_time:
                max_time = total_time

            dist_sum += r['distance']

        x_dist.append(dist_sum)
        x.append(max_time)



    print_stats("total_time", x)
    print_stats("total_dist", x_dist)

    ax_scatter.scatter(x, x_dist, s = 10, facecolor='#00693c')
    ax_scatter.set_title(title)
    ax_scatter.set_xlabel(u'Czas wykonania [s]')
    ax_scatter.set_xlim(left = 0, right = MAX_TIME)
    ax_scatter.set_ylim(bottom = 0, top = MAX_DIST)

    x.extend([MAX_TIME] * fail_count)

    if len(x_dist) == 0:
        x_dist = [None]

    ax.hist(x, bins = 30, range=(0, MAX_TIME), facecolor='#00693c', alpha=1, align='mid')
    ax.set_xlabel(u'Czas wykonania [s]')
    ax.set_xlim(left = 0, right = MAX_TIME)
    ax.set_title(title)

    ax_dist.hist(x_dist, bins = 30, range=(0, MAX_DIST), facecolor='#00693c', alpha=1, align='mid')
    ax_dist.set_xlabel(u'Suma przebytych odległości [m]')
    ax_dist.set_xlim(left = 0, right = MAX_DIST)
    ax_dist.set_title(title)

    print

if __name__ == '__main__':
    client = MongoClient()
    collection = client[MONGO_DB][MONGO_COLLECTION]

    fig = plt.figure(figsize = (8, 4), dpi=150)
    ax1 = fig.add_subplot(121)
    ax2 = fig.add_subplot(122)

    fig_dist = plt.figure(figsize = (8, 4), dpi=150)
    ax1_dist = fig_dist.add_subplot(121)
    ax2_dist = fig_dist.add_subplot(122)

    fig_scatter = plt.figure(figsize = (8, 4), dpi=150)
    ax1_scatter = fig_scatter.add_subplot(121)
    ax2_scatter = fig_scatter.add_subplot(122)

    ax1_scatter.set_ylabel(u'Suma przebytych odległości [m]')
    ax1.set_ylabel(u'Liczba wystąpień')
    ax1_dist.set_ylabel(u'Liczba wystąpień')

    compute(client, "SimpleAlgorithm", ax1, ax1_dist, ax1_scatter, u"Losowe priorytety")
    compute(client, "FearfulAlgorithm", ax2, ax2_dist, ax2_scatter, u"Propagacja straszności")

    fig_dist.tight_layout()
    fig.tight_layout()
    fig_scatter.tight_layout()
    plt.show()

