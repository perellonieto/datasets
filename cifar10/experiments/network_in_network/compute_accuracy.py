#!/usr/bin/python

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import numpy as np

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('probabilities',
                        help='Output probabilities of the model')
    parser.add_argument('labels',
                        help='True labels')

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = parse_args()
    probabilities = np.genfromtxt(args.probabilities, delimiter=',')
    labels = np.genfromtxt(args.labels, delimiter=',')

    N = len(labels)
    accuracy = 0.0
    for prob, label in zip(probabilities, labels):
        #print np.argmax(prob)
        #print int(label)
        #exit()
        #print("{} {}".format(prob, label))
        if np.argmax(prob) == int(label):
            accuracy += 1
    accuracy /= N
    print(accuracy)
