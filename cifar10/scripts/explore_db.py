#!/usr/bin/python

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import numpy as np
import leveldb
import lmdb
import caffe

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('dataset',
                        help='Path to the database')

    parser.add_argument('-csv', '--csv_file',
                        help='Export to a csv file',
                        default=None)

    parser.add_argument('-db', '--db-type',
                        help='Type of database',
                        default='lmdb')
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

    x = []
    if args.db_type == 'leveldb':
        print('Reading LevelDB database {}'.format(args.dataset))
        env = leveldb.LevelDB(args.dataset)
        cursor = env.RangeIter()
        for key, value in cursor:
            datum.ParseFromString(value)
            if args.label:
                x.append(np.asarray(datum.label, dtype=np.int))
            else:
                x.append(np.asarray(datum.float_data))
    else:
        print('Reading lmdb database {}'.format(args.dataset))
        env = lmdb.open(args.dataset, readonly=True)
        with env.begin() as txn:
            cursor = txn.cursor()
            for key, value in cursor:
                datum.ParseFromString(value)
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
