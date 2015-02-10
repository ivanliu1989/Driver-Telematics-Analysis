#!/usr/bin/env python
#
# 2014/12/17 - Fabio fmagalhaes@gmail.com
#

from math import erf, sqrt
from os import listdir, path
from sys import argv
import gzip

SQRT_2 = 1.4142135623730951

def islist(l):
    return type(l) == list

def vectorize(f, X):
    return map(f, X) if islist(X) else f(X)

def cdf(X, mean=0, std=1):
    """
    Cumulative distribution function.
    """
    return vectorize(lambda x: 0.5 * (1 + erf((x - mean)/(std*SQRT_2))), X)

def scale(X, mean, sdev):
    """
    R like scale function.
    """
    return vectorize(lambda x: (x-mean)/sdev, X)

def labs(X):
    return vectorize(abs, X) 

def minus(X):
    return vectorize(lambda x: -x, X)

def zscore(X, mean, sdev):
    return vectorize(lambda x: 2*x, cdf(minus(labs(scale(X, mean, sdev)))))

def sd(X, mean):
    return sqrt(sum(map(lambda x: pow(x - mean, 2), X)) / (len(X)-1))

def projection(collection, name):
    return map(lambda x: x[name], collection)

def edist(a, b):
    """
    Euclidean distance
    """
    return sqrt(pow(b[0]-a[0], 2) + pow(b[1]-a[1], 2))

def read_lines(filename):
    f = open(filename)
    lines = f.read().splitlines()
    f.close()
    # Strip out header.
    return lines[1:]

def parse_line(line):
    x, y = line.split(',')
    return [float(x), float(y)]

def read_trip(file_path, trip):
    a = None
    b = 0.0
    total = 0.0
    stats = {}

    f = path.join(file_path, trip)
    for n, line in enumerate(read_lines(f)):
        point = parse_line(line)
       
        # In case is the first observation.
        if a is None:
            a = point
            continue

        b = point
        dist = edist(a, b)
        total += dist
        a = b

    stats['points'] = n
    stats['distance'] = total
    stats['trip'] = trip.split('.')[0]

    return stats

def read_driver_trips(filepath, driver_dir):
    """
    Read all trips of one driver (directory).
    """
    files = listdir(path.join(filepath, driver_dir))

    trip_stats = [read_trip(path.join("drivers", driver_dir), trip) for trip in files]
    
    n = len(trip_stats)
    total_driver_distance = 0.0

    for t in trip_stats:
        total_driver_distance += t['distance']

    mean = total_driver_distance/n

    sdev = sd(projection(trip_stats, 'distance'), mean)

    for t in trip_stats:
        t['prob'] = zscore(t['distance'], mean, sdev)
        t['driver'] = driver_dir

    return trip_stats
        
def main(data_path):
    alldirs = listdir(data_path)
    output = gzip.open("submission.gz", "w")
    output.write("driver_trip,prob\n")
    n = 0

    # Read each driver trips
    for driver_dir in alldirs:
        if n%250 == 0:
            print "n =", n

        trips = read_driver_trips(data_path, driver_dir)
        for t in trips:
            output.write("%s_%s,%f"%(t['driver'], t['trip'], t['prob']))
            output.write("\n")

        n += 1

    output.close()

if __name__ == "__main__":
    data_path = "drivers"

    if len(argv) > 1:
        data_path = argv[1]

    main(data_path)

