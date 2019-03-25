#!/bin/bash

./wipeout.sh
./config/scripts/makemake
./configure \
    --with-linalg-flavor='custom' \
    --with-linalg-libs="-L$(realpath ..)/lapack-mkl/build/lib \
                        -L$PACT_BENCH/local/lib64 -L$PACT_BENCH/local/lib \
                        -llapack -lblas \
                        -lcuda -lcublas -lcudart -lpthread -ldl -lcudashim -lpager"
make multi multi_nprocs=$(nproc)
