#!/bin/bash

subroutines=($(ag --nobreak --no-heading '^\s*subroutine' BLAS/SRC | \
  tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))

files=($(find -name '*.f'))

nsubs=${#subroutines[*]}
nfiles=${#files[*]}
ntot=$(($nsubs * $nfiles))

i=0
for f in ${files[@]}; do
  echo -en "\r$i/$nfiles"
  comm=""
  for sub in ${subroutines[@]}; do
    comm+="s/$sub/A$sub/Ig;"
  done
  sed -i "$comm" "$f"
  i=$((i+1))
done
