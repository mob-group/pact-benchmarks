CFLAGS=-std=c99

CUDAFLAGS=-I$(PACT_BENCH)/local/include
CUDALINK=-L$(PACT_BENCH)/local/lib -L$(PACT_BENCH)/local/lib64 -lcuda -lcublas -lcudart -lpthread -ldl -lpager

SHIM_INSTALL ?= $(PACT_BENCH)/local/lib

FC=gfortran

.PHONY: all tests clean install
all: tests libcudashim.a

libcudashim.a: shim.o
	ar cru $@ $^
	ranlib $@

tests: test ftest

test: test.c libcudashim.a
	$(CC) $(CFLAGS) $(CUDAFLAGS) $< -o $@ $(CUDALINK) -L. -lcudashim

ftest: ftest.f90 libcudashim.a
	$(FC) $< -o $@ $(CUDALINK) -L. -lcudashim

%.o : %.c
	$(CC) $(CFLAGS) $(CUDAFLAGS) -c $^ -o $@

install: libcudashim.a
	mkdir -p $(SHIM_INSTALL)
	cp $^ $(SHIM_INSTALL)

clean:
	rm -f *.a *.o test ftest
