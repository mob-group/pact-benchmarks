#!/bin/bash

files=($(find -name '*.F'))

# subroutines=($(ag --nobreak --no-heading '^\h*subroutine' ${files[@]} | \
#   tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))
# fns=($(ag --nobreak --no-heading '^.*FUNCTION' ${files[@]} | tr -s ' ' | \
#        cut -d'(' -f1 | rev | cut -d' ' -f1 | rev | \
#        sed 's/\.\.//g;s/arguments//g;s/argument//g' | sort -u | tail +2))
# subroutines+=("${fns[@]}")
subroutines=(chetrd_hb2st cuncsd dorcsd dsytrd_sb2st iparam2stage sorcsd ssytrd_sb2st zhetrd_hb2st zuncsd dlamc3)

nsubs=${#subroutines[*]}
nfiles=${#files[*]}
ntot=$(($nsubs * $nfiles))

comm=""
for sub in ${subroutines[@]}; do
  comm+="s/([^_csdz])$sub/\1AB_$sub/Ig;"
done

function rename_one() {
  f="$1"
  ftrunc "$f" > "$f.tmp"
  mv "$f.tmp" "$f"
}

sed -r -i "$comm" "${files[@]}"

for f in "${files[@]}"; do
  rename_one "$f"
done
