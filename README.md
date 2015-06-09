FPTaylor: A Tool for Rigorous Estimation of Round-off Floating-point Errors
===========================================================================

Demo
----

Try out FPTaylor without installing it at
[Aptlab](https://www.aptlab.net/p/FPTaylor/FPTaylorVM)

Publications
------------

- A. Solovyev, C. Jacobsen, Z. Rakamaric, G. Gopalakrishnan,
[*Rigorous Estimation of Floating-Point Round-off Errors with Symbolic Taylor Expansions*](http://soarlab.org/2015/04/fm2015-sjrg/). (FM 2015)

- C. Jacobsen, A. Solovyev, G. Gopalakrishnan,
[*A parametrized Floating-Point Formalization in HOL Light*](http://www.cs.utah.edu/fv/papers/nsv15-fp-hol-light.pdf). (NSV 2015)

Linux Setup
-----------

Steps below have been tested on Ubuntu 12.04 and Ubuntu 14.04.

Requirements:

- [OCaml](http://ocaml.org/) with a native compiler (do
  not forget `make opt` if you are building from source code). 
  The easiest way to install OCaml on Ubuntu is `sudo apt-get install ocaml`

- Interval computation library for OCaml. It is distributed with FPTaylor 
  (see [INTERVAL](INTERVAL))

- [Maxima](http://maxima.sourceforge.net), a Computer Algebra System
  (optional but recommended).
  The easiest way to install Maxima on Ubuntu is `sudo apt-get install maxima`

- [Z3](https://github.com/Z3Prover/z3) (optional)

- [Gelpia](https://github.com/keram88/gelpia) (optional)


The following command will build FPTaylor and the interval computation library:

    make all

The executable FPTaylor file is called `fptaylor`. It is recommended
to create the environment variable `FPTAYLOR_BASE` which contains the
path to the base FPTaylor directory.  If this environment variable is
created, then it will be possible to copy `fptaylor` to different
places or to create a symbolic link to it. The environment variable
can be created with the following commands (in bash):

    export FPTAYLOR_BASE=$(pwd)
    echo "export FPTAYLOR_BASE=$(pwd)" >> ~/.bashrc

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
