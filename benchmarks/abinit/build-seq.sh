#!/bin/bash

./wipeout.sh
./config/scripts/makemake
./configure \
    --with-linalg-flavor='custom' \
    --with-linalg-libs="-L$(realpath ..)/lapack-3.8.0/build/lib -llapack -lblas"
make -j$(nproc)
