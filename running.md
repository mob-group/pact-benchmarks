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

Get the collected benchmarking code:
```
cd $PACT_BENCH
git clone git@github.com:Baltoli/locksteps.git
cd locksteps/benchmarks
cd lapack-3.8.0
mkdir build
cd build
cmake ..
make -j$(nproc)
```

Install cublas:
```
curl -OL https://developer.nvidia.com/compute/cuda/8.0/Prod2/local_installers/cuda_8.0.61_375.26_linux-run
sh cuda_8.0.61_375.26_linux-run
# follow the script
rm cuda_8.0.61_375.26_linux-run
```
