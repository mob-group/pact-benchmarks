```
export PACT_BENCH=$HOME/pact-bench
mkdir -p $PACT_BENCH
mkdir -p $PACT_BENCH/local
cd $PACT_BENCH
```

Install openmpi:
```
cd $PACT_BENCH
curl -O http://www.mpich.org/static/downloads/3.3/mpich-3.3.tar.gz
tar xvf mpich-3.3.tar.gz
rm -f mpich-3.3.tar.gz
cd mpich-3.3/
./configure --prefix="$PACT_BENCH/local"
make -j$(nproc)
make install
export PATH=$PACT_BENCH/local/bin:$PATH
```

```
cd $PACT_BENCH
curl -OL https://download.open-mpi.org/release/open-mpi/v4.0/openmpi-4.0.0.tar.bz2
tar xvf openmpi-4.0.0.tar.bz2
cd openmpi-4.0.0
./configure CFLAGS=-std=c99 --prefix=$PACT_BENCH/local
make -j$(nproc)
make install
export PATH=$PACT_BENCH/local/bin:$PATH
```

Get the collected benchmarking code:
```
cd $PACT_BENCH
git clone git@github.com:Baltoli/locksteps.git
```

Build the two BLAS implementations:
```
versions=("3.8.0" "mkl")
for v in "${versions[@]}"; do
  cd "$PACT_BENCH/locksteps/benchmarks/lapack-$v"
  mkdir build
  cd build
  cmake ..
  make -j$(nproc)
done
```

Install cublas:
```
cd $PACT_BENCH
curl -OL https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run
sh cuda_8.0.61_375.26_linux-run
# follow the script
rm cuda_8.0.61_375.26_linux-run
```

Install MKL:
```
cd $PACT_BENCH
curl -OL http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/14895/l_mkl_2019.1.144.tgz
tar xvf l_mkl_2019.1.144.tgz
cd l_mkl_2019.1.144
./install.sh
# follow the script
cd ..
rm -rf l_mkl_2019.1.144 l_mkl_2019.1.144.tgz
source $PACT_BENCH/local/compilers_and_libraries_2019/linux/mkl/bin/mklvars.sh intel64
```

Install OpenBLAS:
```
cd $PACT_BENCH
curl -OL https://github.com/xianyi/OpenBLAS/archive/v0.3.5.tar.gz
tar xvf v0.3.5.tar.gz
cd OpenBLAS-0.3.5
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=$PACT_BENCH/local ..
make -j$(nproc)
make install
```

Install CLBlast:
```
cd $PACT_BENCH
git clone git@github.com:CNugteren/CLBlast.git
cd CLBlast
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=$PACT_BENCH/local ..
make -j$(nproc)
make install
```
