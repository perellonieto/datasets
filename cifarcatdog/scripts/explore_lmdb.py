#!/usr/bin/python

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import numpy as np
import lmdb
import caffe

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('lmdb_dataset',
                        help='Path to the lmdb database')

    parser.add_argument('-csv', '--csv_file',
                        help='Export to a csv file',
                        default=None)

    parser.add_argument('-label', '--label',
                        help='Use the label',
                        dest='label', action='store_true')
    parser.add_argument('-nolabel', '--no-label',
                        help='Do not use the label',
                        dest='label', action='store_false')
    parser.set_defaults(param=False)

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = parse_args()

    datum = caffe.proto.caffe_pb2.Datum()

    env = lmdb.open(args.lmdb_dataset, readonly=True)
    x = []
    with env.begin() as txn:
        cursor = txn.cursor()
        for key, value in cursor:
            datum.ParseFromString(value)
#            print("{}, {}".format(np.array2string(np.asarray(datum.float_data),
#                                                 separator=','),
#                                 datum.label))
            if args.label:
                x.append(np.asarray(datum.label, dtype=np.int))
            else:
                x.append(np.asarray(datum.float_data))
    x = np.vstack(x)
    if args.csv_file != None:
        if args.label:
            np.savetxt(args.csv_file, x, fmt='%i', delimiter=",")
        else:
            np.savetxt(args.csv_file, x, fmt='%.5f', delimiter=",")
    else:
        print(x)
