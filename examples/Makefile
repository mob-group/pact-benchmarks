EXAMPLES = \
  nwchem_gemm nwchem_gemm_gemv nwchem_scal_axpy nwchem_dlarfx

all: $(EXAMPLES:=.good.bc)

%.bc : %.F
	$(FLANG) -c -emit-llvm $^ -o $@

%.good.bc : %.bc
	$(IDL_CLANG) -O2 -fno-unroll-loops -fno-vectorize -fno-slp-vectorize -c -emit-llvm $^ -o $@

clean:
	rm -f *.bc *.ll
