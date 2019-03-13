MKLFLAGS=-g -DMKL_ILP64 -m64 -I${MKLROOT}/include
MKLLINK=-lgomp -lpthread -lm -ldl

test: main.o libmklshim.a
	$(CC) $(MKLFLAGS) $^ -o $@ $(MKLLINK)

libmklshim.a: mkl_shim.o
	ar crsT $@ ${MKLROOT}/lib/intel64/libmkl_intel_ilp64.a ${MKLROOT}/lib/intel64/libmkl_gnu_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a $^

%.o : %.c
	$(CC) $(MKLFLAGS) $^ -c -o $@

clean:
	rm -f *.a *.o test
