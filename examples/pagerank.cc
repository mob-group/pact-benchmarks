#include <dlfcn.h>

#include <algorithm>
#include <chrono>
#include <iostream>
#include <map>
#include <random>
#include <string>
#include <vector>

namespace mm {

enum class field { real, double_, complex_, integer, pattern };
enum class symmetry { general, symmetric, skew_symmetric, hermitian };

class matrix {
 public:
  virtual double operator()(size_t x, size_t y) const = 0;
  virtual size_t rows() const = 0;
  virtual size_t cols() const = 0;
};

class coordinate_matrix : public matrix {
 public:
  double operator()(size_t x, size_t y) const override;
  size_t rows() const override;
  size_t cols() const override;

  std::map<std::pair<size_t, size_t>, double> const &entries() const;

  static coordinate_matrix read_from_string(std::string const &data);
  static coordinate_matrix read_from_file(std::string const &filename);

  void normalise();

 private:
  coordinate_matrix() = default;

  void process_line(std::string const &line, symmetry sym, field f);

  size_t rows_ = 0;
  size_t cols_ = 0;
  std::map<std::pair<size_t, size_t>, double> entries_ = {};
};

struct one_based_index_t {};
one_based_index_t one_based_index;

class csr_matrix : public matrix {
 public:
  explicit csr_matrix(coordinate_matrix const &coo);
  csr_matrix(one_based_index_t tag, coordinate_matrix const &coo);

  double operator()(size_t x, size_t y) const override;
  size_t rows() const override;
  size_t cols() const override;

  std::vector<double> const &values() const;
  std::vector<int> const &rowptr() const;
  std::vector<int> const &colidx() const;

  size_t nnz() const;

  void scale(double d);

 private:
  csr_matrix(size_t offset, coordinate_matrix const &coo);

  std::vector<double> values_;
  std::vector<int> rowptr_;
  std::vector<int> colidx_;

  size_t offset_;
  size_t rows_;
  size_t cols_;
  size_t nnz_ = 0;
};

struct csr {
  csr(csr_matrix const &);
  ~csr();

  double const *const a;
  int const *const rowstr;
  int const *const colidx;
  int const *const rows;
};

}  // namespace mm

class dynamic_library {
 public:
  dynamic_library(const std::string &path) {
    lib_ = dlopen(path.c_str(), RTLD_NOW);
    if (!lib_) {
      throw std::runtime_error(dlerror());
    }
  }

  ~dynamic_library() {
    if (lib_) {
      dlclose(lib_);
    }
  }

  template <typename Func>
  Func *symbol(const std::string &symbol) {
    dlerror();
    void *sym = dlsym(lib_, symbol.c_str());
    char *err = dlerror();
    if (err) {
      throw std::runtime_error(err);
    }

    return reinterpret_cast<Func *>(sym);
  }

 private:
  void *lib_;
};

std::string extract_implementation(std::string library);
std::string extract_matrix(std::string path);

struct benchmark_stats {
  std::string path;
  std::vector<double> times;
  size_t rows;
  size_t cols;
  size_t nnz;
  size_t iters;
};

std::ostream &operator<<(std::ostream &os, benchmark_stats const &stats) {
  os << "," << extract_matrix(stats.path);
  for (auto time : stats.times) {
    os << "," << time;
  }

  return os;
}

std::vector<double> random_starting_vector(size_t rows) {
  std::vector<double> x(rows, 0);
  auto &&rd = std::random_device{};
  auto dist = std::uniform_real_distribution<>(0, 1.0);

  std::generate(x.begin(), x.end(), [&rd, &dist] { return dist(rd); });

  auto sum = std::accumulate(x.begin(), x.end(), 0.0);
  std::for_each(x.begin(), x.end(), [sum](auto &val) { val /= sum; });

  return x;
}

template <typename Func>
benchmark_stats run_benchmark(Func &&harness, std::string const &path) {
  auto mat = mm::coordinate_matrix::read_from_file(path);
  if (mat.rows() != mat.cols()) {
    throw std::runtime_error("Pagerank needs a square matrix!");
  }
  mat.normalise();

  auto csr = mm::csr_matrix(mm::one_based_index, mat);
  auto d = 0.85;
  csr.scale(d);

  auto x = random_starting_vector(csr.rows());
  auto y = std::vector<double>(csr.rows(), 0);

  auto raw = mm::csr(csr);

  volatile double error;
  std::vector<double> last_vector;

  auto iters = 1024u;
  auto runs = 5;
  auto times = std::vector<double>{};

  for (auto run = 0; run < runs; ++run) {
    auto start = std::chrono::system_clock::now();

    for (auto i = 0; i < iters; ++i) {
      error = 0.0;

      last_vector = x;

      auto mean = std::accumulate(x.begin(), x.end(), 0.0) / x.size();
      auto add_term = (1 - d) * mean;

      /* v = (d * M) * v + ((1 - d)) * v.average(); */
      /* error = (v - last_v).l2norm(); */
      /* harness(y.data(), raw.a, x.data(), raw.rowstr, raw.colidx, raw.rows);
       */

      for (int i = 0; i < *raw.rows; i++) {
        double value = 0.0;
        for (int j = raw.rowstr[i]; j < raw.rowstr[i + 1]; j++) {
          value += raw.a[j] * x[raw.colidx[j]];
        }
        y[i] = value;
      }

      std::for_each(y.begin(), y.end(),
                    [add_term](auto &out) { out += add_term; });

      x = y;
      for (auto i = 0; i < csr.rows(); ++i) {
        error += std::pow(x.at(i) - last_vector.at(i), 2);
      }
      error = std::sqrt(error);
    }

    auto end = std::chrono::system_clock::now();
    std::chrono::duration<double> time = end - start;

    times.push_back(time.count());
  }

  return {path, times, csr.rows(), csr.cols(), csr.nnz(), iters};
}

using harness_t = void(double *, const double *, const double *, const int *,
                       const int *, const int *);

void usage(char **argv) {
  std::cerr << "Usage: " << argv[0] << " library"
            << " label"
            << " [benchmarks...]\n";
  std::cerr << "Arguments:\n";
  std::cerr << "  library: Path to a shared library with a compatible "
               "spmv_harness_ implementation\n";
  std::cerr << "  label: Text label to be printed at the end of each row to "
               "identify the implementation used\n";
  std::cerr << "  benchmarks: One or more paths to matrix market files to "
               "benchmark\n";
}

int pagerank_main(int argc, char **argv) {
  if (argc < 3) {
    usage(argv);
    return 1;
  }

  auto dyn = dynamic_library(argv[1]);
  auto harness = dyn.symbol<harness_t>("spmv_harness_");

  for (int i = 3; i < argc; ++i) {
    auto path = std::string(argv[i]);
    auto stats = run_benchmark(harness, path);
    std::cout << argv[2] << ",PageRank," << extract_implementation(argv[1])
              << /* matrix */ stats << '\n';
  }

  return 0;
}
