//
// TODO: This script extracts one sample per class from the CIFAR training
// dataset
// Usage:
//    convert_cifar_data
// The CIFAR dataset could be downloaded at
//    http://www.cs.toronto.edu/~kriz/cifar.html

#include <fstream>  // NOLINT(readability/streams)
#include <string>

#include "boost/scoped_ptr.hpp"
#include "glog/logging.h"
#include "google/protobuf/text_format.h"
#include "stdint.h"

#include "caffe/proto/caffe.pb.h"
#include "caffe/util/db.hpp"

using caffe::Datum;
using boost::scoped_ptr;
using std::string;
namespace db = caffe::db;

const int kCIFARSize = 32;
const int kCIFARImageNBytes = 3072; // 32*32*3
const int kCIFARBatchSize = 10000;
const int kCIFARTrainBatches = 5;
const int CAT_LABEL = 3;
const int DOG_LABEL = 5;

void read_image(std::ifstream* file, int* label, char* buffer) {
  char label_char;
  file->read(&label_char, 1);
  *label = label_char;
  file->read(buffer, kCIFARImageNBytes);
  return;
}

void sample_dataset(const string& input_folder, const string& output_folder) {
  // Data buffer
  int label;
  char str_buffer[kCIFARImageNBytes];

  LOG(INFO) << "Reading Training data";
  for (int fileid = 0; fileid < kCIFARTrainBatches; ++fileid) {
    // Open files
    LOG(INFO) << "Training Batch " << fileid + 1;
    snprintf(str_buffer, kCIFARImageNBytes, "/data_batch_%d.bin", fileid + 1);
    std::ifstream data_file((input_folder + str_buffer).c_str(),
        std::ios::in | std::ios::binary);
    CHECK(data_file) << "Unable to open train file #" << fileid + 1;
    for (int itemid = 0; itemid < kCIFARBatchSize; ++itemid) {
      read_image(&data_file, &label, str_buffer);
      if (label == CAT_LABEL or label == DOG_LABEL) {
          label = (label == CAT_LABEL) ? 0 : 1;
          datum.set_label(label);
          datum.set_data(str_buffer, kCIFARImageNBytes);
          int length = snprintf(str_buffer, kCIFARImageNBytes, "%05d",
              fileid * kCIFARBatchSize + itemid);
          string out;
          CHECK(datum.SerializeToString(&out));
      }
    }
  }
}

int main(int argc, char** argv) {
  if (argc != 3) {
    printf("This script converts the CIFAR dataset to the leveldb format used\n"
           "by caffe to perform classification.\n"
           "Usage:\n"
           "   Extracts one sample per class from the training CIFAR dataset.\n"
           "The CIFAR dataset could be downloaded at\n"
           "   http://www.cs.toronto.edu/~kriz/cifar.html\n"
           "You should gunzip them after downloading.\n");
  } else {
    google::InitGoogleLogging(argv[0]);
    sample_dataset(string(argv[1]), string(argv[2]));
  }
  return 0;
}
