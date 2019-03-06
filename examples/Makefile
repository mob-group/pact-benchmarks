NWCHEM = \
  nwchem_gemm nwchem_gemm_gemv nwchem_scal_axpy nwchem_dlarfx

BLAS = \
  daxpy dgemm dgemv dger dscal

EXAMPLES = \
  nwchem.inline.bc

all: $(EXAMPLES)

install: all
	mkdir -p out
	cp $(EXAMPLES) out

nwchem.bc: $(BLAS:=.bc) $(NWCHEM:=.bc)
	$(IDL_BIN)/llvm-link $^ -o $@

%.bc : %.F
	$(FLANG) -Wno-override-module -c -emit-llvm $^ -o $@

%.inline.bc : %.fixed.bc
	$(IDL_BIN)/opt -inline -inline-threshold=10000 $^ -o $@

%.fixed.bc : %.bc
	$(IDL_BIN)/clang -O2 \
	  -fno-unroll-loops -fno-vectorize -fno-slp-vectorize \
	  -c -emit-llvm $^ -o $@

clean:
	rm -f *.bc *.ll
	rm -f replace-report*
	rm -rf out
