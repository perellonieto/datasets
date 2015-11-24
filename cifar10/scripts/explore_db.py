#!/usr/bin/python

from sys import exit
from os import rename, remove
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

    parser.add_argument('-f', '--force',
                        help='Rewrite the output csv file',
                        default=False, action='store_true')

    parser.add_argument('-db', '--db-type',
                        help='Type of database',
                        default='lmdb')
    parser.add_argument('-label', '--label',
                        help='Use the label',
                        dest='label', action='store_true')
    parser.add_argument('-nolabel', '--no-label',
                        help='Do not use the label',
                        dest='label', action='store_false')
    parser.add_argument('-mb', '--mini-batch',
                        help='Explore the dataset in batches of the' \
                             'specified size', default=1000, type=int)

    parser.set_defaults(param=False)

    args = parser.parse_args()
    return args


def write_to_csv(cursor, datum, mini_batch, csv_filename):
    x = []
    i = 0
    for key, value in cursor:
        datum.ParseFromString(value)
        if args.label:
            x.append(np.asarray(datum.label, dtype=np.int))
        else:
            x.append(np.asarray(datum.float_data))
        i+=1
        if i%mini_batch:
            x = np.vstack(x)
            if args.csv_file != None:
                with open(csv_filename, 'a') as f_handle:
                    if args.label:
                        np.savetxt(f_handle, x, fmt='%i', delimiter=",")
                    else:
                        np.savetxt(f_handle, x, fmt='%.5f', delimiter=",")
            else:
                print(x)
            x = []

    x = np.vstack(x)
    if args.csv_file != None:
        with open(csv_filename, 'a') as f_handle:
            if args.label:
                np.savetxt(f_handle, x, fmt='%i', delimiter=",")
            else:
                np.savetxt(f_handle, x, fmt='%.5f', delimiter=",")
    else:
        print(x)

if __name__ == "__main__":
    args = parse_args()

    datum = caffe.proto.caffe_pb2.Datum()

    if args.force and args.csv_file:
        rename(args.csv_file, '.explore_db_'+args.csv_file)
    else:
        exit("The file {} already exists. Force rewrite by adding the flag" \
              " -f|--force".format(args.csv_file))

    if args.db_type == 'leveldb':
        print('Reading LevelDB database {}'.format(args.dataset))
        env = leveldb.LevelDB(args.dataset)
        cursor = env.RangeIter()
        write_to_csv(cursor, datum, args.mini_batch, args.csv_file)
    else:
        print('Reading lmdb database {}'.format(args.dataset))
        env = lmdb.open(args.dataset, readonly=True)
        with env.begin() as txn:
            cursor = txn.cursor()
            write_to_csv(cursor, datum, args.mini_batch, args.csv_file)

    if args.force and args.csv_file:
        remove('.explore_db_'+args.csv_file)
