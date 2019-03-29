#include "clblastshim.h"

#include <CL/cl.hpp>

#include <iostream>
#include <vector>

int main()
{
  auto platforms = std::vector<cl::Platform>{};
  auto status = cl::Platform::get(&platforms);
  if(status != CL_SUCCESS) {
    std::cerr << "Couldn't get platforms [" << status << "]\n";
    std::exit(2);
  }

  auto plat = platforms[0];
  auto devices = std::vector<cl::Device>{};
  status = plat.getDevices(CL_DEVICE_TYPE_GPU, &devices);
  if(status != CL_SUCCESS) {
    std::cerr << "Couldn't get devices [" << status << "]\n";
    std::exit(3);
  }

  auto dev = devices[0];

  auto ctx = cl::Context(dev);
  auto queue = cl::CommandQueue(ctx, dev);

  float x = 2;
  auto buf = cl::Buffer(ctx, CL_MEM_COPY_HOST_PTR, sizeof(float), &x, &status);

  float g;
  queue.enqueueReadBuffer(buf, CL_TRUE, 0, sizeof(float), &g);

  std::cout << g << '\n';
}
