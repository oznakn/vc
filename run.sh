#!/usr/bin/env sh

riscv64-linux-gnu-gcc -nostdlib test.S -o test.out && \
  rv-jit test.out
