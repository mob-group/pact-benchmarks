#!/bin/bash

echo "benchmark,dataset,implementation,time"

machine="$1"
shift

for impl in "$@"; do
  "./build-$impl.sh" >/dev/null 2>&1

  cd bench/
  for b in */; do
    cd "$b"
    for ((i = 0; i < 5; i++)); do
      t=$(../../src/98_main/abinit < bench.files | ag 'wall_time' | tr -s ' ' | cut -d' ' -f2)
      echo "abinit,${b%/},$impl,$t"
    done
    cd ..
  done
  cd ..
done

git clean -fx bench/ >/dev/null 2>&1
