FPTaylor Benchmarks
-------------------

All benchmarks are in the `macro` directory.

The commands

    make taylor-a
    make taylor-trans-a

will run FPTaylor with the simplified rounding and the approximate
optimization problem on all benchmarks.

The commands

    make taylor-b
    make taylor-trans-b

will run FPTaylor with the improved rounding and the exact optimization
problem on all benchmark.

Polynomial and rational benchmarks are from the paper
[*Sound Compilation of Reals*](http://doi.acm.org/10.1145/2535838.2535874)
by E. Darulova and V. Kuncak.

Additional Examples
-------------------

Additional simple examples can be found in [micro](micro) and
[micro2](micro2) directories.

Use the following commands to run individual examples:

    cd micro2
    ../../fptaylor -c config-rel.cfg sqrt_sub.txt

Here, `config-rel.cfg` is a configuration file with FPTaylor options
and `sqrt_sub.txt` is an input file.

See [FPTaylor Reference Manual](../REFERENCE.md) for a detailed
description of FPTaylor.