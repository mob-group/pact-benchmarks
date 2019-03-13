MKLFLAGS=-std=c99 -DMKL_ILP64 -m64 -I${MKLROOT}/include
MKLLINK=-lgomp -lpthread -lm -ldl

FC=gfortran
SHIM_INSTALL ?= $(PACT_BENCH)/local/lib

all: test ftest

test: main.o libmklshim.a
	$(CC) $(MKLFLAGS) $^ -o $@ $(MKLLINK)

ftest: fort.o libmklshim.a
	$(FC) $(MKLFLAGS) $^ -o $@ $(MKLLINK)

libmklshim.a: mkl_shim.o
	ar crsT $@ ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a $^

%.o : %.c
	$(CC) $(MKLFLAGS) $^ -c -o $@

%.o : %.f90
	$(FC) $(MKLFLAGS) $^ -c -o $@

install: libmklshim.a
	mkdir -p $(SHIM_INSTALL)
	cp $^ $(SHIM_INSTALL)

clean:
	rm -f *.a *.o test ftest
