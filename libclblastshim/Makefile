SLIB=libclblastshim.so

CXXFLAGS := -std=c++11 -isystem$(PACT_BENCH)/local/include $(CXXFLAGS)
LDFLAGS=-L$(PACT_BENCH)/local/lib -L$(PACT_BENCH)/local/lib64 -lOpenCL

.PHONY: all install clean

all: $(SLIB) test

$(SLIB) : lib.cpp clblastshim.h
	$(CXX) $(CXXFLAGS) -fPIC -shared $^ -o $@ $(LDFLAGS)

test: test.cpp $(SLIB)
	$(CXX) $(CXXFLAGS) -L. -lclblastshim -Wl,-rpath,. $^ -o $@ $(LDFLAGS)

install: $(SLIB) clblastshim.h
	mkdir -p $(PACT_BENCH)/local/lib 
	mkdir -p $(PACT_BENCH)/local/include
	cp $(SLIB) $(PACT_BENCH)/local/lib
	cp clblastshim.h $(PACT_BENCH)/local/include

clean:
	rm -rf *.so *.o *.a
	rm -f test
