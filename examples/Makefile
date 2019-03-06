NWCHEM = \
  nwchem_gemm nwchem_gemm_gemv nwchem_scal_axpy nwchem_dlarfx

ABINIT = \
  abinit_A abinit_B

WALES = \
  commons porfuncs NGTmem NGT Pfold

PARBOIL = \
  parboil_spmv

BLAS = \
  daxpy dgemm dgemv dger dscal

EXAMPLES = \
  nwchem.inline.bc \
  abinit.fixed.bc \
  wales.fixed.bc \
  parboil.fixed.bc

all: $(EXAMPLES)

install: all
	mkdir -p out
	cp $(EXAMPLES) out

nwchem.bc: $(BLAS:=.bc) $(NWCHEM:=.bc)
	$(IDL_BIN)/llvm-link $^ -o $@

abinit.bc: $(ABINIT:=.bc)
	$(IDL_BIN)/llvm-link $^ -o $@

wales.bc: $(WALES:=.bc)
	$(IDL_BIN)/llvm-link $^ -o $@

parboil.bc: $(PARBOIL:=.bc)
	$(IDL_BIN)/llvm-link $^ -o $@

%.bc : %.F
	$(FLANG) -Wno-override-module -c -emit-llvm $^ -o $@

%.bc : %.f
	$(FLANG) -Wno-override-module -c -emit-llvm $^ -o $@

%.bc : %.f90
	$(FLANG) -Wno-override-module -c -emit-llvm $^ -o $@

%.bc : %.c
	$(IDL_BIN)/clang -Xclang -disable-O0-optnone -c -emit-llvm $^ -o $@

%.inline.bc : %.fixed.bc
	$(IDL_BIN)/opt -inline -inline-threshold=10000 $^ -o $@

%.fixed.bc : %.bc
	$(IDL_BIN)/clang -O2 \
	  -fno-unroll-loops -fno-vectorize -fno-slp-vectorize \
	  -c -emit-llvm $^ -o $@

clean:
	rm -f *.bc *.ll
	rm -f replace-report*
	rm -f *.mod
	rm -rf out
