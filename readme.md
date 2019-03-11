# Locksteps

This repo contains extracted code samples that can have our compiler tooling run
over them, as well as a collection of runnable benchmarks for the different
examples.

We would like the benchmarks to be as end-to-end as possible where they can be,
and the code samples will be extracted for simplicity of running Philip's
tooling over them.

## Examples

The example code samples we have are:

* NWChem
* Abinit
* Parboil
* Pathsample
* GAPBS
* Darknet
* UTDSP
* DSPStone

## Benchmarks

The runnable benchmarks we have are:

### NWChem

* Building:
  * Needs `mpif90` installed to compile properly - seems to usually come from an
    OpenMPI development package and can be easily installed on cluster / local
    machines.
  * Environment variables need to be set properly `export
    NWCHEM_TOP=.../locksteps/benchmarks/nwchem-6.8.1
    NWCHEM_TARGET=LINUX64 NWCHEM_MODULES=all USE_INTERNALBLAS=y USE_MPI=yes`
* Running:
  * Built executable is `bin/nwchem` - it reads input from files included with
    the package.
  * Files that provide good benchmarks are:
  * To switch BLAS implementations:
