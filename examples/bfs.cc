#include <algorithm>
#include <chrono>
#include <cmath>
#include <ctime>
#include <fstream>
#include <iostream>
#include <random>
#include <vector>

#ifndef _LIBRARY_
#define _LIBRARY_
#include <iostream>
#include <vector>

class CSRMatrix {
 public:
  int rows;
  int columns;
  std::vector<int> rowstr;
  std::vector<int> colidx;
  std::vector<double> values;

  void print() const;

  int get_cols() const { return columns; }
};

class COOMatrix {
 public:
  struct Value {
    int x;
    int y;
  };

  static COOMatrix read(std::istream&);

  int rows;
  int columns;
  std::vector<Value> values;

  void print() const;

  void normalize();

  CSRMatrix convert();
};

class Vector {
 public:
  Vector(size_t N = 0) : values(N) {}

  static Vector rand(size_t N);

  double l1norm() const;
  double l2norm() const;
  double average() const;

  std::vector<double> values;
};

Vector operator*(double a, const Vector& b);

Vector operator+(const Vector& a, const Vector& b);

Vector operator+(const Vector& a, double b);

Vector operator-(const Vector& a, const Vector& b);

CSRMatrix operator*(double a, const CSRMatrix& b);

Vector operator*(const CSRMatrix& m, const Vector& v);

#endif

void COOMatrix::normalize() {
  std::vector<int> column_sums(columns);

  for (size_t i = 0; i < values.size(); i++) {
    if (values[i].x != values[i].y) {
      column_sums[values[i].y - 1] += 1;
    } else {
      values[i] = values.back();
      values.pop_back();
      i--;
    }
  }

  for (int i = 0; i < column_sums.size(); i++) {
    if (column_sums[i] == 0) {
      for (int j = 0; j < rows; j++) {
        if (i != j) values.push_back(Value{j + 1, i + 1});
      }
    }
  }
}

CSRMatrix COOMatrix::convert() {
  /*
    std::vector<double> column_sums(columns);

    for(int i = 0; i < values.size(); i++)
    {
        column_sums[values[i].y-1] += 1;
    }

    std::vector<std::vector<int>> vectors(rows);

    for(int i = 0; i < values.size(); i++)
    {
        vectors[values[i].x-1].push_back(values[i].y-1);
    }

    for(int i = 0; i < rows; i++)
    {
        std::sort(vectors[i].begin(), vectors[i].end());
    }*/

  CSRMatrix result;
  result.rows = rows;
  result.columns = columns;
  result.rowstr.push_back(1);

  size_t current_row = 0;
  size_t running_total = 0;

  for (auto i = 0u; i < values.size(); ++i) {
    for (; current_row < values.at(i).x; ++current_row) {
      result.rowstr.push_back(running_total + 1);
    }

    running_total++;
    result.colidx.push_back(values.at(i).y + 1);
    result.values.push_back(1.0);
  }

  for (; current_row < rows; ++current_row) {
    result.rowstr.push_back(running_total + 1);
  }

  /*
  for(int i = 0; i < rows; i++)
  {
      if(vectors[i].size() == 0)
      {
          for(int j = 0; j < columns; j++)
          {
              result.colidx.push_back(j);
              result.values.push_back(1.0);
          }
      }
      else
      {
          for(int j = 0; j < vectors[i].size(); j++)
          {
              if(j == 0 || vectors[i][j] != vectors[i][j-1])
              {
                  result.colidx.push_back(vectors[i][j]);
                  result.values.push_back(1.0);
              }
              else
              {
                  result.values.back() += 1.0;
              }
          }
      }

      result.rowstr.push_back(result.values.size());
  }

  for(int i = 0; i < result.values.size(); i++)
  {
      result.values[i] /= column_sums[result.colidx[i]];
  }*/

  return result;
}

void COOMatrix::print() const {
  std::vector<std::vector<double>> matrix(rows,
                                          std::vector<double>(columns, 0.0));

  for (int i = 0; i < values.size(); i++) {
    matrix[values[i].x - 1][values[i].y - 1] += 1.0;
  }

  std::cout.precision(1);

  std::cout << "Begin COO Matrix\n";
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < columns; j++) {
      std::cout << " " << std::fixed << matrix[i][j];
    }
    std::cout << "\n";
  }
  std::cout << "End COO Matrix\n";
}
void CSRMatrix::print() const {
  std::vector<std::vector<double>> matrix(rows,
                                          std::vector<double>(columns, 0.0));

  for (int i = 0; i < rows; i++) {
    for (int j = rowstr[i]; j < rowstr[i + 1]; j++)
      matrix[i][colidx[j]] = values[j];
  }

  std::cout.precision(3);

  std::cout << "Begin CSR Matrix\n";
  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < columns; j++) {
      std::cout << " " << std::fixed << matrix[i][j];
    }
    std::cout << "\n";
  }
  std::cout << "End CSR Matrix\n";
}

COOMatrix COOMatrix::read(std::istream& istr) {
  size_t size;
  COOMatrix result;

  istr >> result.rows >> result.columns >> size;

  result.values.resize(size);

  for (int i = 0; i < size; i++) {
    istr >> result.values[i].x >> result.values[i].y;
  }

  return result;
}

Vector Vector::rand(size_t N) {
  Vector result(N);

  for (int i = 0; i < N; i++)
    result.values[i] = (double)::rand() / (double)RAND_MAX;

  return result;
};

double Vector::l2norm() const {
  double value = 0.0;
  for (int i = 0; i < values.size(); i++) value += values[i] * values[i];
  return sqrt(value);
}

double Vector::l1norm() const {
  double value = 0.0;
  for (int i = 0; i < values.size(); i++) value += values[i];
  return value;
}

double Vector::average() const {
  double value = 0.0;
  for (int i = 0; i < values.size(); i++) value += values[i];
  return value / (double)values.size();
}

Vector operator*(double a, const Vector& b) {
  Vector result(b.values.size());

  for (int i = 0; i < b.values.size(); i++) result.values[i] = a * b.values[i];

  return result;
}

Vector operator+(const Vector& a, const Vector& b) {
  Vector result(a.values.size());

  for (int i = 0; i < a.values.size(); i++)
    result.values[i] = a.values[i] + b.values[i];

  return result;
}

Vector operator+(const Vector& a, double b) {
  Vector result(a.values.size());

  for (int i = 0; i < a.values.size(); i++) result.values[i] = a.values[i] + b;

  return result;
}

Vector operator-(const Vector& a, const Vector& b) {
  Vector result(a.values.size());

  for (int i = 0; i < a.values.size(); i++)
    result.values[i] = a.values[i] - b.values[i];

  return result;
}

CSRMatrix operator*(double a, const CSRMatrix& b) {
  CSRMatrix result = b;

  for (int i = 0; i < b.values.size(); i++) result.values[i] *= a;

  return result;
}

Vector operator*(const CSRMatrix& m, const Vector& v) {
  Vector out(m.rows);

  for (int i = 0; i < m.rows; i++) {
    double value = 0.0;
    for (int j = m.rowstr[i]; j < m.rowstr[i + 1]; j++) {
      value += m.values[j] * v.values[m.colidx[j]];
    }
    out.values[i] = value;
  }

  return out;
}

Vector operator&(const Vector& A, const Vector& B) {
  Vector result(A.values.size());

  for (int i = 0; i < result.values.size(); i++) {
    result.values[i] = (A.values[i] != 0) && (B.values[i] != 0);
  }

  return result;
}

Vector operator~(const Vector& A) {
  Vector result(A.values.size());

  for (int i = 0; i < result.values.size(); i++) {
    result.values[i] = (A.values[i] == 0);
  }

  return result;
}

// https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=7046157
Vector BFS(const CSRMatrix& M, int s) {
  size_t N = M.rows;
  Vector front(N);
  Vector distances(N);

  for (int i = 0; i < N; i++) {
    front.values[i] = 0;
    distances.values[i] = 0;
  }

  front.values[s] = 1;
  distances.values[s] = 1;

  bool cont = true;
  for (int i = 1; cont; i++) {
    front = M * front & ~distances;

    cont = false;
    for (int j = 0; j < front.values.size(); j++) {
      if (front.values[j]) {
        cont = true;
        distances.values[j] = i + 1;
      }
    }
  }

  return distances;
}

int bfs_main(int args, char** argv) {
  COOMatrix coo_mat = COOMatrix::read(std::cin);

  //    coo_mat.print();

  CSRMatrix csr_mat = coo_mat.convert();

  //    csr_mat.print();

  std::random_device rd{};
  std::uniform_int_distribution<> dis(0, csr_mat.columns - 1);

  auto start = std::chrono::system_clock::now();

  auto iters = 16;
  for (auto i = 0; i < iters; ++i) {
    auto s = dis(rd);
    Vector result = BFS(csr_mat, s);
    volatile double result_val = result.values.at(0);
  }

  auto end = std::chrono::system_clock::now();
  std::chrono::duration<double> time = end - start;

  std::cout << time.count() << "\n";
  /*
      std::cout<<"Page rank result:\n";

      for(int i = 0; i < result.values.size(); i++)
      {
          std::cout<<i+1<<":\t"<<result.values[i]<<"\n";
      }
  */

  return 0;
}
