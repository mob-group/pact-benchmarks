#include <clblast.h>

#include <CL/cl.hpp>

#include <iostream>

using namespace clblast;

class sgemm_functor {
public:
  sgemm_functor()
  {
    auto platforms = std::vector<cl::Platform>{};
    auto status = cl::Platform::get(&platforms);
    if(status != CL_SUCCESS) {
      throw std::runtime_error("platforms");
    }

    auto platform = platforms[0];

    auto devices = std::vector<cl::Device>{};
    status = platform.getDevices(CL_DEVICE_TYPE_GPU, &devices);
    if(status != CL_SUCCESS) {
      throw std::runtime_error("devices");
    }

    auto device = devices[0];

    ctx = cl::Context(device);
    queue = cl::CommandQueue(ctx, device);
  }

  void operator()(int m, int n, int k, float alpha,
                  float *A, int lda, 
                  float *B, int ldb,
                  float *C, int ldc)
  {
    if(m != last_m || n != last_n || k != last_k) {
      last_m = m;
      last_n = n;
      last_k = k;

      A_buf = cl::Buffer(ctx, CL_MEM_COPY_HOST_PTR, m * k * sizeof(float), A);
      B_buf = cl::Buffer(ctx, CL_MEM_COPY_HOST_PTR, k * n * sizeof(float), B);
      C_buf = cl::Buffer(ctx, CL_MEM_WRITE_ONLY, m * n * sizeof(float));
    }

    auto stat = Gemm<float>(Layout::kRowMajor, Transpose::kNo, Transpose::kNo, 
        m, n, k, alpha, 
        A_buf(), 0, lda,
        B_buf(), 0, ldb, 0.0,
        C_buf(), 0, ldc, &queue());

    queue.enqueueReadBuffer(C_buf, true, 0, m * n * sizeof(float), C);
  }

private:
  cl::Context ctx;
  cl::CommandQueue queue;

  cl::Buffer A_buf;
  cl::Buffer B_buf;
  cl::Buffer C_buf;

  int last_m = 0;
  int last_n = 0;
  int last_k = 0;
};

extern "C" {

void blast_sgemm_(
  int m, int n, int k, float alpha,
  float *A, int lda,
  float *B, int ldb,
  float *C, int ldc
)
{
  static sgemm_functor impl{};
  impl(m, n, k, alpha, A, lda, B, ldb, C, ldc);
}

}
