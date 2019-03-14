#!/bin/bash

files=($(find -regex '.*\.[fF]\(90\)?'))

subroutines=($(ag --nobreak --no-heading '^\h*subroutine' ${files[@]} | \
  tr -s ' ' | cut -d' ' -f3 | cut -d '(' -f1 | sort -u))
fns=($(ag --nobreak --no-heading '^.*FUNCTION' ${files[@]} | tr -s ' ' | \
       cut -d'(' -f1 | rev | cut -d' ' -f1 | rev | \
       sed 's/\.\.//g;s/arguments//g;s/argument//g' | sort -u | tail +2))
subroutines+=("${fns[@]}")
subroutines+=(chetrd_hb2st cuncsd dorcsd dsytrd_sb2st iparam2stage sorcsd ssytrd_sb2st zhetrd_hb2st zuncsd dlamc3)

for sub in "${subroutines[@]}"; do
  echo "$sub" | sed 's/AB_//g'
done | sort -u
