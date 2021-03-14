FPTaylor: A Tool for Rigorous Estimation of Round-off Floating-point Errors
===========================================================================

You can run FPTaylor without installation at 
[FPTaylor JS's website](https://monadius.github.io/FPTaylorJS).

Publications
------------

- A. Solovyev, M. Baranowski, I. Briggs, C. Jacobsen, Z. Rakamaric, G. Gopalakrishnan [*Rigorous Estimation of Floating-Point Round-off Errors with Symbolic Taylor Expansions*](https://soarlab.org/publications/2018_toplas_sbbjrg/). (ACM Transactions on Programming Languages and Systems)

- A. Solovyev, C. Jacobsen, Z. Rakamaric, G. Gopalakrishnan,
[*Rigorous Estimation of Floating-Point Round-off Errors with Symbolic Taylor Expansions*](https://soarlab.org/publications/2015_fm_sjrg/). (FM 2015)

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

macOS and Linux Setup
---------------------

Steps below have been tested on Ubuntu 12.04, Ubuntu 14.04, Ubuntu 16.04, macOS 10.14, 
and macOS 10.15.

The following command will build FPTaylor and the interval computation library:

    make all

If you encounter a problem during the build process then you may try
steps described in the next section (alternative interval arithmetic library).
The executable FPTaylor file is called `fptaylor`. It is recommended to
create the environment variable `FPTAYLOR_BASE` which contains the
path to the base FPTaylor directory.  If this environment variable is
created, then it will be possible to copy `fptaylor` to different
places or to create a symbolic link to it. The environment variable
can be created with the following commands (in bash):

    export FPTAYLOR_BASE=$(pwd)
    echo "export FPTAYLOR_BASE=$(pwd)" >> ~/.bashrc

Alternative Interval Arithmetic Library
---------------------------------------

The interval computation library [INTERVAL](INTERVAL) may be incompatible with
some operating systems and processors. There is an alternative interval arithmetic
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
direction as well: FPTaylor can translate its input files into FPCore benchmarks.
This translation is done with the export tool which can be built with the following
command:

    make export-tool

Then it is possible to run

    ./export -o output_file.fpcore input_file

See [Reference](REFERENCE.md#creating-fpcore-benchmarks) for additional information.

Reference
---------

A detailed description of FPTaylor can be found in
[REFERENCE.md](REFERENCE.md)

Formal Verification of FPTaylor Results in HOL Light
----------------------------------------------------

See the [formal](formal) directory for corresponding HOL Light
theories and procedures.

JavaScript
----------

FPTaylor can be compiled to JavaScript with [Js_of_ocaml](https://ocsigen.org/js_of_ocaml).

Install Js_of_ocaml and ocamlfind:

```
opam install js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt ocamlfind
```

Then run

```
make clean
make fptaylor-js
```

The file `fptaylor.js` will be created in the project directory. 
This command also creates `default_config.js` which exports a string representation
of the default config file (with some required modifications).

It is also possible to create a debug version of `fptaylor.js`:

```
make clean
make fptaylor-js-debug
```

JavaScript Examples
-------------------

Copy `fptaylor.js` and `default_config.js` to the [js](js) directory.
Delete the `export` statement from `js/default_config.js`.
Then start a local server:

```
cd js
python3 -m http.server
```

Now open `localhost:8000` in your browser.

A full-featured [website](https://monadius.github.io/FPTaylorJS) is also available.
