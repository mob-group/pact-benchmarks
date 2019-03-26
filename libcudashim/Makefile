CFLAGS=-std=c99
CXXFLAGS=-std=c++11

CUDAFLAGS=-I$(PACT_BENCH)/local/include
CUDALINK=-L$(PACT_BENCH)/local/lib -L$(PACT_BENCH)/local/lib64 -lcuda -lcublas -lcudart -lpthread -ldl

SHIM_INSTALL ?= $(PACT_BENCH)/local/lib

FC=gfortran

.PHONY: all tests clean install
all: tests libcudashim.a

libcudashim.a: shim.o
	ar cru $@ $^
	ranlib $@

tests: test ftest

test: test.cpp libcudashim.a
	$(CXX) $(CXXFLAGS) $(CUDAFLAGS) $< -o $@ -L. -lcudashim $(CUDALINK)

ftest: ftest.f90 libcudashim.a
	$(CXX) $< -o $@ $(CUDALINK) -lgfortran -L. -lcudashim

%.o : %.c
	$(CC) $(CFLAGS) $(CUDAFLAGS) -c $^ -o $@

%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(CUDAFLAGS) -c $^ -o $@

install: libcudashim.a
	mkdir -p $(SHIM_INSTALL)
	cp $^ $(SHIM_INSTALL)

clean:
	rm -f *.a *.o test ftest
