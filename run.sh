#!/usr/bin/env sh

# riscv64-linux-gnu-gcc -nostdlib test.S -o test && \
#  rv-jit test.out
#
#rm -f test.o

riscv64-linux-gnu-as test.s -o test.o && \
  riscv64-linux-gnu-ld test.o -o test.out && \
  rv-sim test.out

rm -f test.o