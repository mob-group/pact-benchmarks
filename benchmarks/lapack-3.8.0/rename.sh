#!/bin/bash

subroutines=($(ag --nobreak --no-heading '^\s*subroutine' BLAS/SRC SRC | \
  tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))
fns=($(ag --nobreak --no-heading '^.*FUNCTION' BLAS/SRC | tr -s ' ' | \
  rev | cut -d' ' -f1 | rev | cut -d '(' -f1 | sort -u))
subroutines+=("${fns[@]}")

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
  echo $comm
  sed -i "$comm" "$f"
  ftrunc "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
  i=$((i+1))
done
