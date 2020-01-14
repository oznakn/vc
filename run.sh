#!/usr/bin/env sh

riscv64-linux-gnu-as -march=rv64imac -o test.o test.s
riscv64-linux-gnu-ld -o test test.o
rv-jit test
rm -f test test.o
