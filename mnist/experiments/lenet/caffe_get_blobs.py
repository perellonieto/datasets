#!/usr/bin/python

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import caffe
from caffe.proto import caffe_pb2
from google.protobuf import text_format

def get_pooling_types_dict():
    """Get dictionary mapping pooling type number to type name
    """
    desc = caffe_pb2.PoolingParameter.PoolMethod.DESCRIPTOR
    d = {}
    for k, v in desc.values_by_name.items():
        d[v.number] = k
    return d


def print_blob_names(net):
    separator = "\t"
    for layer in net.layer:
        print layer.name
        if layer.type == 'Convolution' or layer.type == 'Deconvolution':
            # Outer double quotes needed or else colon characters don't parse
            # properly
            node_label = '"%s%s(%s)%skernel size: %d%sstride: %d%spad: %d"' %\
                         (layer.name,
                          separator,
                          layer.type,
                          separator,
                          layer.convolution_param.kernel_size[0] if len(layer.convolution_param.kernel_size._values) else 1,
                          separator,
                          layer.convolution_param.stride[0] if len(layer.convolution_param.stride._values) else 1,
                          separator,
                          layer.convolution_param.pad[0] if len(layer.convolution_param.pad._values) else 0)
        elif layer.type == 'Pooling':
            pooling_types_dict = get_pooling_types_dict()
            node_label = '"%s%s(%s %s)%skernel size: %d%sstride: %d%spad: %d"' %\
                         (layer.name,
                          separator,
                          pooling_types_dict[layer.pooling_param.pool],
                          layer.type,
                          separator,
                          layer.pooling_param.kernel_size,
                          separator,
                          layer.pooling_param.stride,
                          separator,
                          layer.pooling_param.pad)
        else:
            node_label = '"%s%s(%s)"' % (layer.name, separator, layer.type)
        print node_label

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('input_net_proto_file',
                        help='Input network prototxt file')

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = parse_args()
    caffe.set_mode_cpu()
    net = caffe_pb2.NetParameter()
    text_format.Merge(open(args.input_net_proto_file).read(), net)
    print_blob_names(net)
