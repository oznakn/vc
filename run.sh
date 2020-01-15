#!/usr/bin/env sh

riscv64-linux-gnu-as "examples/$1.s" -o "examples/$1.o" && \
  riscv64-linux-gnu-ld "examples/$1.o" -o "examples/$1.out" && \
  rv-sim "examples/$1.out"

rm -f "examples/$1.o" "examples/$1.out"