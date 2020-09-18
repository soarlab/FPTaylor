FPTaylor: A Tool for Rigorous Estimation of Round-off Floating-point Errors
===========================================================================

Publications
------------

- A. Solovyev, C. Jacobsen, Z. Rakamaric, G. Gopalakrishnan,
[*Rigorous Estimation of Floating-Point Round-off Errors with Symbolic Taylor Expansions*](http://soarlab.org/2015/04/fm2015-sjrg/). (FM 2015)

- C. Jacobsen, A. Solovyev, G. Gopalakrishnan,
[*A parametrized Floating-Point Formalization in HOL Light*](http://www.cs.utah.edu/fv/papers/nsv15-fp-hol-light.pdf). (NSV 2015)

Requirements
-------------

- [OCaml](http://ocaml.org/) version 4.03.0 or higher with a native compiler (do
  not forget `make opt` if you are building from source code). 
  A recommended way to install OCaml is with [OPAM](https://opam.ocaml.org/doc/Install.html)

- (Included) Interval computation library for OCaml. It is distributed with
  FPTaylor (see [INTERVAL](INTERVAL)). There is also an alternative
  interval arithmetic library in [simple_interval](simple_interval)
  which has limited functionality but it is portable to different
  systems.

- (Optional) [Gelpia](https://github.com/soarlab/gelpia)

- (Optional) [Z3](https://github.com/Z3Prover/z3)

- (Optional) [Maxima](http://maxima.sourceforge.net), a Computer Algebra System
  The easiest way to install Maxima on Ubuntu is `sudo apt-get install maxima`

Linux Setup
-----------

Steps below have been tested on Ubuntu 12.04, Ubuntu 14.04, and Ubuntu 16.04.

The following command will build FPTaylor and the interval computation library:

    make all

If you encounter a problem during the build process then you may try
steps described in the next section (macOS and Linux Setup).  The
executable FPTaylor file is called `fptaylor`. It is recommended to
create the environment variable `FPTAYLOR_BASE` which contains the
path to the base FPTaylor directory.  If this environment variable is
created, then it will be possible to copy `fptaylor` to different
places or to create a symbolic link to it. The environment variable
can be created with the following commands (in bash):

    export FPTAYLOR_BASE=$(pwd)
    echo "export FPTAYLOR_BASE=$(pwd)" >> ~/.bashrc
	
macOS and Linux Setup
---------------------

The main issue with macOS (and some newer versions of Linux) is that
the interval computation library [INTERVAL](INTERVAL) cannot be
compiled on this system. There is an alternative interval arithmetic
library distributed with FPTaylor (see [simple_interval](simple_interval)).
This library is under development so it does not support all functions
of the [INTERVAL](INTERVAL) library. In particular, trigonometric
functions are not supported yet.

The following command will build FPTaylor with the simple interval
library:

    make fptaylor-simple-interval
	
Benchmarks and Examples
-----------------------

Benchmarks and examples are in the [benchmarks](benchmarks) directory.

FPBench Support
---------------

[FPBench](http://fpbench.org/) is a benchmark suite and a collection of tools for
the floating-point research community. FPBench supports FPTaylor and it has a tool
for converting FPCore benchmarks into FPTaylor input files. This works in the other 
direction as well: FPTaylor can translate its input files into FPCore benchmarks 
(see FPTaylor's reference manual).

Reference
---------

A detailed description of FPTaylor can be found in
[REFERENCE.md](REFERENCE.md)

Formal Verification of FPTaylor Results in HOL Light
----------------------------------------------------

See the [formal](formal) directory for corresponding HOL Light
theories and procedures.

VirtualBox Image
----------------

An old version of FPTaylor is available as a VirtualBox image at
[https://sites.google.com/site/fptaylordemo/installation](https://sites.google.com/site/fptaylordemo/installation)
