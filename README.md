FPTaylor: A Tool for Rigorous Estimation of Round-off Floating-point Errors
===========================================================================

Demo
----

Try out FPTaylor without installing it at
[Aptlab](https://www.aptlab.net/p/FPTaylor/FPTaylorVM)

Linux Setup
-----------

Requirements:

- [OCaml](http://ocaml.org/) with a native compiler (do
  not forget `make opt` if you are building from source code). 
  The easiest way to install OCaml on Ubuntu is
    sudo apt-get install ocaml

- [Z3](https://github.com/Z3Prover/z3) (optional)

- Interval computation library for OCaml. It is distributed with FPTaylor 
  (see [INTERVAL](INTERVAL))

The following command will build FPTaylor and the interval computation library:

    make all

Benchmarks and Examples
-----------------------

Benchmarks and examples are in the [benchmarks](benchmarks) directory.

Reference
---------

A detailed description of FPTaylor can be found in
[REFERENCE.md](REFERENCE.md)

Formal Verification of FPTaylor Results in HOL Light
----------------------------------------------------

See the [formal](formal) directory for corresponding HOL Light
theories and procedures.
