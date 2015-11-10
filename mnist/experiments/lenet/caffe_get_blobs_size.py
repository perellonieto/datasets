#!/usr/bin/python

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import caffe
from caffe.proto import caffe_pb2
from google.protobuf import text_format

def print_blob_names(net):
    for layer in net.blobs.keys():
        total_size = reduce(lambda x, y: x*y, net.blobs[layer].data.shape)
        print "{}\t{}\t= {}".format(layer, net.blobs[layer].data.shape,
                                  total_size)

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('input_net_proto_file',
                        help='Input network prototxt file')
    parser.add_argument('input_net_snapshot_file',
                        help='Input network snapshot file')

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = parse_args()
    caffe.set_mode_cpu()
    net = caffe_pb2.NetParameter()
    text_format.Merge(open(args.input_net_proto_file).read(), net)
    print "First argument: {}".format(args.input_net_proto_file)
    print "Second argument: {}".format(args.input_net_snapshot_file)
    net = caffe.Classifier(args.input_net_proto_file,
                           args.input_net_snapshot_file, caffe.TEST)
    print_blob_names(net)
