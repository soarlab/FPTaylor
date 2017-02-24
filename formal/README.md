Verification of FPTaylor Certificates in HOL Light
==================================================

Setup
-----

Requirements:

- [HOL Light](http://www.cl.cam.ac.uk/~jrh13/hol-light/).
  The following [installation script](https://bitbucket.org/akrauss/hol-light-workbench) 
  can help to install HOL Light and compatible versions of OCaml and Camlp5.

  The script installs an old version of OCaml. It is not difficult to install HOL Light
  if you have [OPAM](https://opam.ocaml.org):

  1) Run `opam install camlp5`

  2) `git clone https://github.com/jrh13/hol-light`

  3) Run `make` inside the HOL Light directory

- [Nonlinear inequalities verification tool.](https://github.com/monadius/formal_ineqs)
  Clone or download its source code to any directory on your machine.

- [DMTCP](http://dmtcp.sourceforge.net/index.html) (Optional but recommended.)

It is recommended to use checkpointing (DMTCP) with HOL Light in order
to avoid reloading all theory files every time a new HOL Light session
is started. The setup process below assumes that DMTCP is installed.

1) Edit the file `load.hl`. In this file, enter correct paths to your installations of
   the inequality verification tool and FPTaylor.

2) Run a DMTCP coordinator in a separate terminal:

    dmtcp_coordinator

3) In another terminal window, go to the HOL Light directory and type

    dmtcp_launch ocaml

4) Now type

    #use "hol.ml";;

   If you get an error that `camlp5` libraries are not found
   then exit (`#quit;;`) and run the following commands:

    dmtcp_launch ocaml -I `camlp5 -where`
    #use "hol.ml";;

5) Wait approximately 2 minutes until HOL Light core libraries are loaded. Type

    needs "absolute_path_to_fptaylor_formal_directory/load.hl"

In the command above, make sure that the path to the file `load.hl` is correct.

6) It may take up to 2 hours before all required files are loaded
(most time is required for loading HOL Light multivariate analysis
theory files). When everything is loaded, go to the DMTCP coordinator terminal and type

    c

Press ENTER and wait until a checkpointed image of HOL Light with all
loaded libraries is created. This file will be created in the same
directory where the DMTCP coordinator was started. Rename the
checkpointed image (it is a file with the extension `dmtcp`) to
`dmtcp_fptaylor.dmtcp`.

7) Now you may quit HOL Light with the command

    #quit;;

In order to restart HOL Light with all loaded libraries, it is enough to type

    dmtcp_restart dmtcp_fptaylor.dmtcp

(Make sure that the path to dmtcp_fptaylor.dmtcp is correct.)


Verification of FPTaylor Certificates
-------------------------------------

First of all, it is necessary to prepare FPTaylor proof certificates. Go to `FPTaylor/benchmarks/proofs` and run the commands

    make tests
    make tests2
    make taylor-a
    make trans-a
    make taylor-c

These commands will create proof certificates for all FPTaylor benchmarks in the directory `FPTaylor/benchmarks/proofs/proofs`.

It is possible to prepare a proof certificate for an individual benchmark:

    ../../fptaylor -c config.cfg sqrt_sub.txt

All proof certificates are saved in the `proof` directory created
inside the current working directory. Names of proof certificate files
correspond to names of expressions inside FPTaylor input files. Proof
certificates are created when the configuration file (`config.cfg` in
the example above) contains the line 

    proof-record = true

It is also important to have `fp-power2-model = false` because the
current version of the formal verification procedure does not support
the advanced rounding model.

Restart HOL Light with the command

    dmtcp_restart dmtcp_fptaylor.dmtcp

The following commands will verify some simple certificates:

    needs "tests/exp1x.hl";;
    needs "tests/hypot.hl";;
    needs "tests/sphere.hl";;
    needs "tests/sqrt_sub.hl";;

The next command will verify all FPTaylor benchmarks with polynomial and
rational functions:

    needs "tests/benchmarks_a.hl";;

It requires about 1 hour to complete this verification.

The command

    needs "tests/benchmarks_c.hl";;

verifies the same problems as the previous command but with better error bounds
(because exact optimization problems are verified: `opt-exact = true`). 

The command

    needs "tests/benchmarks_trans_a.hl";;

verifies FPTaylor transcendental benchmarks.  Verification of the
benchmark `azimuth` takes about 5 hours.