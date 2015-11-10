#!/usr/bin/python

import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import caffe
from caffe.proto import caffe_pb2
from google.protobuf import text_format
from math import ceil

LAYERS_WITH_BLOB = ['Convolution', 'Pooling', 'Concat', 'InnerProduct', 'LRN',
                    'Accuracy', 'Softmax', 'SoftmaxWithLoss']

def get_pooling_types_dict():
    """Get dictionary mapping pooling type number to type name
    """
    desc = caffe_pb2.PoolingParameter.PoolMethod.DESCRIPTOR
    d = {}
    for k, v in desc.values_by_name.items():
        d[v.number] = k
    return d


def print_blob_names(net, params=True):
    separator = " "
    for layer in net.layer:
        if layer.type in LAYERS_WITH_BLOB:
            if params:
                if layer.type == 'Convolution' or layer.type == 'Deconvolution':
                    k = layer.convolution_param.kernel_size[0] if len(layer.convolution_param.kernel_size._values) else 1
                    s = layer.convolution_param.stride[0] if len(layer.convolution_param.stride._values) else 1
                    p = layer.convolution_param.pad[0] if len(layer.convolution_param.pad._values) else 0
                    blob_size = ceil((2*p-k)/s)+1
                    node_label = '%s%s(%s)%sk: %d%ss: %d%sp: %d = %s' %\
                                 (layer.name,
                                  separator,
                                  layer.type,
                                  separator,
                                  layer.convolution_param.kernel_size[0] if len(layer.convolution_param.kernel_size._values) else 1,
                                  separator,
                                  layer.convolution_param.stride[0] if len(layer.convolution_param.stride._values) else 1,
                                  separator,
                                  layer.convolution_param.pad[0] if len(layer.convolution_param.pad._values) else 0,
                                  blob_size)
                elif layer.type == 'Pooling':
                    pooling_types_dict = get_pooling_types_dict()
                    k = layer.pooling_param.kernel_size
                    s = layer.pooling_param.stride
                    p = layer.pooling_param.pad
                    blob_size = ceil((2*p-k)/s)+1
                    node_label = '%s%s(%s %s)%skernel size: %d%sstride: %d%spad: %d = %s' %\
                                 (layer.name,
                                  separator,
                                  pooling_types_dict[layer.pooling_param.pool],
                                  layer.type,
                                  separator,
                                  layer.pooling_param.kernel_size,
                                  separator,
                                  layer.pooling_param.stride,
                                  separator,
                                  layer.pooling_param.pad,
                                  blob_size)
                else:
                    node_label = '%s%s(%s)' % (layer.name, separator, layer.type)
                print node_label
            else:
                print layer.name

def parse_args():
    """Parse input arguments
    """
    parser = ArgumentParser(description=__doc__,
                            formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument('input_net_proto_file',
                        help='Input network prototxt file')

    parser.add_argument('-param', '--parameters',
                        help='Show parameters of each layer',
                        dest='param', action='store_true')
    parser.add_argument('-noparam', '--no-parameters',
                        help='Do not show parameters of each layer',
                        dest='param', action='store_false')
    parser.set_defaults(param=False)

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = parse_args()
    caffe.set_mode_cpu()
    net = caffe_pb2.NetParameter()
    text_format.Merge(open(args.input_net_proto_file).read(), net)
    print_blob_names(net, args.param)
