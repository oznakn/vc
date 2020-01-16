# vc, A Compiler for V

[![](https://tokei.rs/b1/github/oznakn/vc)](https://github.com/oznakn/vc)

[![](https://travis-ci.org/oznakn/vc.svg?branch=master)](https://travis-ci.org/oznakn/vc)

A compiler for METU CENG444's programming langauge v, for semester 20191. To see the reference of the language, 
click [here](https://github.com/bozsahin/ceng444/blob/a72670446051e0b206cd76a8a2ea169301c6bfce/project-material/vspecs-2019.pdf)

`vc` is built with [Rust](https://www.rust-lang.org/), using the [lalrpop](https://github.com/lalrpop/lalrpop) parser generator, with LALR(1) mode enabled.

For now, `vc` only generates RISC-V 64 bit target code.

### Requirements

* `Rust`
* [riscv-gnu-toolchain](https://github.com/riscv/riscv-gnu-toolchain)
* [rv8](https://rv8.io/)

### Getting Started

In order to run the code, type `cargo run -- compile examples/simple.v`. This will generate `simple.s` file in the `examples` directory.

Then, type `run.sh simple`. This will automatically runs assembler and linker for the assembly file, and runs the target code using the `rv8` simulator.

### Authors

* Ozan Akın
