SLIB=libclblastshim.so

CXXFLAGS := -std=c++11 -I$(PACT_BENCH)/local/include $(CXXFLAGS)

.PHONY: all install clean

all: $(SLIB) test

$(SLIB) : lib.cpp clblastshim.h
	$(CXX) $(CXXFLAGS) -fPIC -shared $^ -o $@

test: test.cpp $(SLIB)
	$(CXX) $(CXXFLAGS) -L. -lclblastshim -Wl,-rpath,. $^ -o $@

install: $(SLIB) clblastshim.h
	cp $(SLIB) $(PACT_BENCH)/local/lib
	cp clblastshim.h $(PACT_BENCH)/local/include

clean:
	rm -rf *.so *.o *.a
	rm -f test
