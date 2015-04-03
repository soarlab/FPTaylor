# FPTaylor Reference Manual

**FPTaylor** is a tool for estimation of round-off errors of
floating-point computations. The following command invokes FPTaylor
on a given input file

    fptaylor input_file_name

See *Input file format* for the description of the input file
format. FPTaylor reads the input file and analyzes expressions defined
in this file. All operations in input files are assumed to be over
real numbers. FPTaylor models floating-point arithmetic with rounding
operations. The basic analysis which FPTaylor performs is the
following. Suppose the input file contains an expression `expr`
containing some variables (for simplicity, assume that is depends on
one variable `x`) and some rounding operations. FPTaylor constructs
another expression `expr'` without rounding operations and estimates
the maximum value of the difference

    |expr - expr'|

over all possible values of `x` (this is the absolute round-off error
of `expr`). FPTaylor also can estimate the value of

    |(expr - expr') / expr|

(the relative round-off error of `expr`) if it finds that `expr` is
never equal to 0 for all values of `x` and if the option for computing
relative errors is turned on.

It is possible to invoke FPTaylor with several files. In this case,
FPTaylor analyzes expressions in all input files in sequence.

Special configuration files can be used to pass parameters to
FPTaylor. To invoke FPTaylor with given configuration files, use the
following command

    fptaylor [-c config_file1 [-c config_file2 ...]] input_file1 [input_file2 ...]

If several configuration files are provided, options in the second
configuration file will override options in the first file, and so
on. The default configuration file `default.cfg` is always loaded
first. See the section *Options* for a complete description of
available options in FPTaylor.


## Input file format

Each **FPTaylor** input files consists of several sections:
*Constants*, *Variables*, *Definitions*, *Constraints*, and
*Expressions*. The section *Constants* defines constants. The section
*Variables* defines variables. The section *Definitions* contains
named expressions (definitions). The section *Constraints* defines
constraints which specify the input domain. The section *Expressions*
contains expressions for analysis. The most important sections are
*Variables* and *Expressions*. Sections must be defined in the order:
*Constants*, *Variables*, *Definitions*, *Expressions*. Any section
may be omitted. FPTaylor will not produce any result for an input file
without the *Expressions* section.

Comments in **FPTaylor** input files have the form

    // this is a comment

### Constants

The section with constants has the following syntax

    Constants
      name = constant expression,
      name = constant expression,
      ...
      name = constant expression;

All constant definitions must be separated by comma and the last
definition must end with a semicolon. It is possible to refer to
previously defined constants in constant definitions. All constants
are assumed to be real rational numbers. It is not possible to define
irrational constants (e.g., a square root of 2: `sqrt(2)`). Irrational
constants may be defined in the *Definitions* section.

Example:

    Constants
      k = 0.1,
      c = 1 / k * 2;

### Variables

The section with variables has the following syntax

    Variables
      type var_name in [low, high],
      ...
      type var_name in [low, high];

All variable definitions must be separated by comma and the last
definition must end with a semicolon. Each variable has a type, name,
and the interval of values. The type can be omitted; in this case, a
variable will get the `real` type.

Variables can be of the following types:

- `real`: the type of real values. This type is assigned by default to
  variables without explicit type.
- `float16`: the type of IEEE-754 16-bit floating-point numbers (half precision).
- `float32`: the type of IEEE-754 32-bit floating-point numbers (single precision).
- `float64`: the type of IEEE-754 64-bit floating-point numbers (double precision).
- `float128`: the type of IEEE-754 128-bit floating-point numbers (quadruple precision).

Names of variables should be different from names of constants.

All variables must be bounded. Bounds are given as rational constant
expressions (support for irrational bounds will be added in future
versions of FPTaylor).

Example:

    Constants
      k = 2;

    Variables
      float32 x in [0, 1],
      y in [2.1 / 3, 20/7 + 0.1],
      real z in [-3, 4.5 + k]; // k is a constant

In the example above, `y` and `z` are real variables, `x` is a single
precision variable.

It is also possible to define a variable which has uncertainties. It
is done in the following way

    type var_name in [low, high] +/- uncertainty

Here, `uncertainty` must be a positive rational constant
expression. Suppose that `x` is a variable in the interval `[a, b]`
with uncertainty `u`. Then we have the following relation between the
actual value (`val(x)`) of `x` and its uncertain value (`x`): `|val(x) - x|
<= u` and `a <= val(x) <= b`. Note that the actual value `val(x)` is
bounded by `a` and `b` (not by `a - u` and `b + u`).


### Definitions

The section with definitions has the following syntax

    Definitions
      def_name = expr,
      ...
      def_name = expr;

All definitions must be separated by comma and the last definition
must end with a semicolon. It is possible to refer to previous
definitions. Definition names must be different from names of
variables and constants.

Example:

    Definitions
      t = x * y + 1,
      z = (t - 1) / (t + 1);

There is an alternative way to write a definition

      def_name rnd= expr

Here, `rnd` must be one of rounding operators. The meaning of this
definition is the following: the rounding operator `rnd` is
recursively applied to subexpressions of `expr`. There is one
restriction: `rnd` is not applied to another rounding operator and it
does not propagate inside another rounding operator.

Example:

    r0 = 1 + x,
    r1 rnd32= x,
    r2 rnd64= x + r0 * y,
    r3 rnd16_up= rnd32(x) + y,
    r4 rnd32= rnd64(x + y);
    

It is equivalent to

    r0 = 1 + x,
    r1 = rnd32(x),
    r2 = rnd64(rnd64(x) + rnd64(rnd64(r0) * rnd64(y))),
    r3 = rnd16_up(rnd32(x) + rnd16_up(y)),
    r4 = rnd64(x + y);

### Constraints

Each variable have lower and upper bounds. Additional constraints can
be defined in the following section

    Constraints
      constr_name: formula,
      ...
      constr_name: formula;

Here `formula` is `expr >= expr` or `expr <= expr`. In the current
version of FPTaylor, constraints are supported with the Z3
optimization backend only.

Example:

    Constraints
      ineq1: a + b >= c;

### Expressions

The section with expressions has the following syntax

    Expressions
      expr_name = expr,
      ...
      expr_name = expr;

All expressions must be separated by comma and the last expression
must end with a semicolon. This section works in the same way as the
*Definitions* section but names of expressions (and the equality sign)
can be omitted. Expressions without names will get automatically
generated names "Expression k" where k is the number of the expression
in the definition list.

It is also possible to use the alternative syntax of definitions with
automatic rounding (see the *Definitions* section).

Example:

    Expressions
      2 * x,	// This expression will be named "Expression 1"
      t rnd16= 2 * x,
      1 + t;   // This expression will be named "Expression 3"

In the example above, the value of the last expression is `1 + rnd16(rnd16(2) * rnd16(x))`.

Names of expressions are printed in the summary section of the FPTaylor output.


## Operations

All operations in **FPTaylor** are assumed to be over real
numbers. Floating-point arithmetic is supported via rounding
operators.

### Basic operations

1. `+` addition
2. `-` subtraction and negation
3. `*` multiplication
4. `/` division
5. `sqrt` square root
6. `fma` (deprecated). `fma(a,b,c)` is equivalent to `a * b + c`. In
early versions of FPTaylor this operation corresponded to `rnd(a * b + c)` 
with the appropriately selected rounding operator.

### Transcendental operations

1. `sin` sine
2. `cos` cosine
3. `exp` exponent
4. `log` logarithm
5. `tan` tangent
6. `atan` arctangent

Other transcendental functions will be added to FPTaylor
soon. Transcendental functions may be not supported by some
optimization backends of FPTaylor. The default optimization backend
(interval branch and bound, `bb`) supports all operations.


## Rounding

The general rounding operator has the following syntax

    rnd(bits, type, scale, eps, delta)

Here, `bits` is one of the following values: 16, 32, 64, or 128. It
specifies the floating-point format to which the operator
rounds. Values of `type` can be: `ne`, `up`, `down`, or `zero`. It
specifies the type of the rounding operator: to nearest, toward
positive infinity (up), toward negative infinity (down), or toward
zero. The value of `scale` must be a real number, values of `eps` and
`delta` must be integers. These values play the following role. Assume
that the following expression is given:

    rnd(bits, type, scale, eps, delta)(f)

FPTaylor creates the following relation between `f` and its rounded value:

    rnd(bits, type, scale, eps, delta)(f) = f + f e + d

with `|e| <= scale * 2^eps` and `|d| <= scale * 2^delta` if `type` is
`ne`, and `|e| <= 2 * scale * 2^eps` and `|d| <= 2 * scale * 2^delta`
if `type` is `up`, `down`, or `zero`. There is one special value for
`eps` and `delta`. If `eps = 0` then `e = 0` and if `delta = 0` then
`d = 0`.

FPTaylor also can work with an improved rounded model where the
expression `f e` is replaced with `p2(f)e`. The function `p2` is a
special function which improves the result of the rounding
approximation. In general, the imporved rounding model leads to more
complicated problems for FPTaylor to solve. It can be turned on with a
special option.

Note that values of `bits` and `type` are not explicitly used in the
above rounded expression. Nevertheless, they play an important role in
the analysis which FPTaylor does internally.

There are several simplified versions of rounding
operators. Parameters `eps` and `delta` can be omitted. In that case,
their values will be deduced automatically from the value of `bits`
and `type`:

    rnd(bits, type, scale)

The parameter `scale` can also be omitted. If it is omitted, then its
value is assumed to be `1.0`.

There are several predefined names for most commonly used rounding operators:

    rnd16 = rnd(16, ne)
    rnd16_up = rnd(16, up)
    rnd16_down = rnd(16, down)
    rnd16_0 = rnd(16, zero)

    rnd32 = rnd(32, ne)
    rnd32_up = rnd(32, up)
    rnd32_down = rnd(32, down)
    rnd32_0 = rnd(32, zero)

    rnd64 = rnd(64, ne)
    rnd64_up = rnd(64, up)
    rnd64_down = rnd(64, down)
    rnd64_0 = rnd(64, zero)

    rnd128 = rnd(128, ne)
    rnd128_up = rnd(128, up)
    rnd128_down = rnd(128, down)
    rnd128_0 = rnd(128, zero)

The special operation `no_rnd` can be applied to any expression. It is
equivalent to the identity operation. It may be useful in the
following context:

    r1 rnd32= x + no_rnd(n - 1);

This example is equivalent to

    r1 = rnd32(rnd32(x) + n - 1);


## Options

Options are specified in configuration files. Base options are defined
in the default configuration file `default.cfg`. A configuration file
`config.cfg` can be loaded with the following command

    fptaylor -c config.cfg input_file1 [input_file2 ...]

Several configuration files can be provided:

    fptaylor -c config1.cfg -c config2.cfg -c config3.cfg ... input_file1 [input_file2 ...]

Options defined in `config2.cfg` override options defined in
`config1.cfg` and in `default.cfg`, options in `config3.cfg` override
options in `config2.cfg`, etc.

The syntax of configuration files is the following:

    option_name = option_value
    ...
    option_name = option_value

Empty lines and lines containing `*` are ignored. Example:

    abs-error = true
    * This is a comment
    This is also * a comment

    rel-error = false

Important options are described below. Other options can be found in `default.cfg`.

### `abs-error`

Possible values: `true`, `false`.

If the value is true, then FPTaylor computes absolute round-off errors of expressions.

### `rel-error`

Possible values: `true`, `false`.

If the value is true, then FPTaylor computes relative round-off errors
of expressions. If the value of an expression can be very close to 0,
then FPTaylor issues a warning and does not compute the relative
error.

### `fp-power2-model`

Possible values: `true`, `false`.

Turns on or off the impoved rounding model. The improved rounding
model yields better error estimation results but it also produces
harder problems for optimization backends to solve. It may be not
supported by some optimization backends.

### `opt-approx`

Possible values: `true`, `false`.

If true, then approximate optimization problems are solved by FPTaylor
optimization backends. These approximate problems are simpler than
exact optimization problems and may yield worse (but still sound)
upper bounds of errors.

### `opt-exact`

Possible values: `true`, `false`.

If true, then exact optimization problems are solved by FPTaylor
optimization backends. These exact problems are harder than
approximate optimization problems. Some optimization backends may not
support exact optimization problems.


### `opt`

Possible values: `bb`, `z3`, and `nlopt`.

Specifies the optimization backend of FPTaylor.

- `bb` is the default optimization backend implemented in FPTaylor. It
  supports all FPTaylor operations and the improved rounding
  model. [OCaml](http://ocaml.org) compiler must be installed in order
  to use this optimization backend.

- `z3` is the optimization backend based on [Z3 SMT
  solver](https://github.com/Z3Prover/z3). Z3 must be installed and its Python
  binding must work. This optimization backend does not support
  transcendental functions, exact optimization problems, and the
  improved rounding model.

- `nlopt` is the optimization backend based on the [NLOpt optimization
  library](http://ab-initio.mit.edu/wiki/index.php/NLopt). This
  optimization backend may yield unsound results but it is fast and is
  a good choice for getting solid preliminary results. This
  optimization backend does not support the improved rounding model
  (will be corrected in future releases of FPTaylor).

### `z3-timeout`

Specifies timeout in milliseconds for Z3 optimization backend.

### `proof-record`

Possible values: `true`, `false`.

If true, then proof certificates are saved for all analyzed
expressions. These proof certificates can be validated with a special
procedure in the HOL Light proof assistant. All proof certificates are
saved in the `proofs` directory (relative to the directory from which
FPTaylor is invoked). In the current version of FPTaylor, proof
certificates cannot be produced for exact optimization problems
(`opt-exact = true`), for the improved rounding model
(`fp-power2-model = true`), and for relative errors (`rel-error =
true`).


## Output

**FPTaylor** prints out information about analyzed expressions in the
standard output. The summary of error analysis is printed at the
end. This summary contains the following information

- The name of the analyzed expression printed as `Problem:
  name_of_the_expression`.

- The absolute round-off error obtained with the exact optimization
  problem if `abs-error = true` and `opt-exact = true`.

- The absolute round-off error obtained with the approximate
  optimization problem if `abs-error = true` and `opt-approx = true`.

- The relative round-off error obtained with the exact optimization
  problem if `rel-error = true` and `opt-approx = true`.

- The relative round-off error obtained with the approximate
  optimization problem if `rel-error = true` and `opt-approx = true`.

- The analysis time in seconds printed as `Elapsed time: time`.

