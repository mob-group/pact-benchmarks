#!/bin/bash

./wipeout.sh
./config/scripts/makemake
./configure \
    --with-linalg-flavor='custom' \
    --with-linalg-libs="-L$(realpath ..)/lapack-mkl/build/lib \
                        -llapack -lblas \
                        -L$PACT_BENCH/local/lib64 \
                        -lopenblas -lpthread"
make multi multi_nprocs=$(nproc)
