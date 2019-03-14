#!/bin/bash

files=($(find -name '*.f'))

subroutines=($(ag --nobreak --no-heading '^\h*subroutine' ${files[@]} | \
  tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))
fns=($(ag --nobreak --no-heading '^.*FUNCTION' ${files[@]} | tr -s ' ' | \
       cut -d'(' -f1 | rev | cut -d' ' -f1 | rev | \
       sed 's/\.\.//g;s/arguments//g;s/argument//g' | sort -u | tail +2))
subroutines+=("${fns[@]}")

nsubs=${#subroutines[*]}
nfiles=${#files[*]}
ntot=$(($nsubs * $nfiles))

comm=""
for sub in ${subroutines[@]}; do
  comm+="s/$sub/AB_$sub/Ig;"
done

i=0

function rename_one() {
  echo -en "\r$i/$nfiles"
  f="$1"
  sed -i "$comm" "$f"
  ftrunc "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
  i=$((i+1))
}

for f in "${files[@]}"; do
  rename_one "$f"
done
