#!/bin/bash

files=($(find -regex '.*\.[fF]\(90\)' -or -name '*.m4'))

comm=""
while read -r line; do
  sub="$line"
  comm+="s/([^_csdz])$sub/\1AB_$sub/Ig;"
done < blasfns.txt

# subroutines=($(ag --nobreak --no-heading '^\h*subroutine' ${files[@]} | \
#   tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))
# fns=($(ag --nobreak --no-heading '^.*FUNCTION' ${files[@]} | tr -s ' ' | \
#        cut -d'(' -f1 | rev | cut -d' ' -f1 | rev | \
#        sed 's/\.\.//g;s/arguments//g;s/argument//g' | sort -u | tail +2))
# subroutines+=("${fns[@]}")
# subroutines=(chetrd_hb2st cuncsd dorcsd dsytrd_sb2st iparam2stage sorcsd ssytrd_sb2st zhetrd_hb2st zuncsd dlamc3)

# nsubs=${#subroutines[*]}
# nfiles=${#files[*]}
# ntot=$(($nsubs * $nfiles))

# comm=""
# for sub in ${subroutines[@]}; do
#   comm+="s/([^_csdz])$sub/\1AB_$sub/Ig;"
# done

# function rename_one() {
#   f="$1"
#   ftrunc "$f" > "$f.tmp"
#   mv "$f.tmp" "$f"
# }

sed -r -i "$comm" "${files[@]}"
sed -i 's/FAAB_LSE/FALSE/g' "${files[@]}"
sed -i 's/EAB_LSE/ELSE/g' "${files[@]}"
sed -i 's/faab_lse/false/g' "${files[@]}"
sed -i 's/eab_lse/else/g' "${files[@]}"

# for f in "${files[@]}"; do
#   rename_one "$f"
# done
