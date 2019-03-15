#!/bin/bash

./wipeout.sh
./config/scripts/makemake
./configure \
    --with-linalg-flavor='custom' \
    --with-linalg-libs="-L$(realpath ..)/lapack-mkl/build/lib \
                        -llapack -lblas -Wl,--start-group \
                        ${MKLROOT}/lib/intel64/libmkl_gf_lp64.a \
                        ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a \
                        ${MKLROOT}/lib/intel64/libmkl_core.a \
                        -Wl,--end-group -lgomp -lpthread -lm -ldl"
make -j$(nproc)
