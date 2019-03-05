%.bc : %.F
	$(FLANG) -c -emit-llvm $^ -o $@

%.good.bc : %.bc
	$(IDL_CLANG) -O2 -fno-unroll-loops -fno-vectorize -fno-slp-vectorize -c -emit-llvm $^ -o $@

clean:
	rm -f *.bc *.ll
