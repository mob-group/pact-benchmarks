CFLAGS=-std=c99 -fPIC -g
CXXFLAGS=-std=c++11 -fPIC -g

CUDAFLAGS=-I$(PACT_BENCH)/local/include
CUDALINK=-L$(PACT_BENCH)/local/lib -L$(PACT_BENCH)/local/lib64 -lcuda -lcublas -lcudart -lpthread -ldl

SHIM_INSTALL ?= $(PACT_BENCH)/local/lib
INC_INSTALL ?= $(PACT_BENCH)/local/include

FC=gfortran

.PHONY: all tests clean install
all: tests libcudashim.so liboldcudashim.so

libcudashim.so: shim.o
	$(CXX) -shared $^ -o $@

liboldcudashim.so: oldshim.o
	$(CC) -shared $^ -o $@

tests: test ftest

test: test.cpp libcudashim.so
	$(CXX) $(CXXFLAGS) $(CUDAFLAGS) $< -o $@ -L. -lcudashim $(CUDALINK) -Wl,-rpath,.

ftest: ftest.f90 libcudashim.so
	$(CXX) $< -o $@ -L. -lcudashim $(CUDALINK) -lgfortran -Wl,-rpath,.

%.o : %.c
	$(CC) $(CFLAGS) $(CUDAFLAGS) -c $^ -o $@

%.o : %.cpp
	$(CXX) $(CXXFLAGS) $(CUDAFLAGS) -c $^ -o $@

install: libcudashim.so liboldcudashim.so shim.h
	mkdir -p $(SHIM_INSTALL)
	mkdir -p $(INC_INSTALL)
	cp libcudashim.so $(SHIM_INSTALL)
	cp liboldcudashim.so $(SHIM_INSTALL)
	cp shim.h $(INC_INSTALL)

clean:
	rm -f *.a *.o test ftest *.so
