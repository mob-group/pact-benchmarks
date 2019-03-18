CFLAGS=-std=c99

CUDAFLAGS=-I$(PACT_BENCH)/local/include
CUDALINK=-L$(PACT_BENCH)/local/lib64 -lcuda -lcublas -lcudart -lpthread -ldl

SHIM_INSTALL ?= $(PACT_BENCH)/local/lib

.PHONY: all
all: libcudashim.a

libcudashim.a: shim.o
	ar cru $@ $^
	ranlib $@

%.o : %.c
	$(CC) $(CFLAGS) $(CUDAFLAGS) -c $^ -o $@

install: libcudashim.a
	mkdir -p $(SHIM_INSTALL)
	cp $^ $(SHIM_INSTALL)

clean:
	rm -f *.a *.o
