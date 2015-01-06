__author__ = 'michal'

from pymongo import MongoClient, errors
import random

MONGO_DB = 'robots'
MONGO_COLLECTION = 'testcases'
ALGORITHM = 'FearfulAlgorithm'
TESTCASE = 'loop'
NUMBER = 100

if __name__ == '__main__':
    client = MongoClient()
    collection = client[MONGO_DB][MONGO_COLLECTION]

    res = collection.find({
            'name': TESTCASE,
            'robots.algorithm': ALGORITHM
            })

    rows = []
    for r in res:
        rows.append(r)

    random.shuffle(rows)
    l = len(rows)

    for r in rows:
        if l > NUMBER:
            collection.remove({"_id" : r['_id']})
            l -= 1


