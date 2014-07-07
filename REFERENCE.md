# FPTaylor Reference Manual

## Input file format
Each **FPTaylor** input files consists of several sections: *Constants*, *Variables*, *Definitions*, *Expressions*. The section *Constants* defines constants. The section *Variables* declares and defines variables. The section *Definitions* contains named expressions (definitions). The section *Expressions* contains expressions for analysis. The most important sections are *Variables* and *Expressions*. Sections must be defined in the order: *Constants*, *Variables*, *Definitions*, *Expressions*. Any section may be omitted. **FPTaylor** will not produce any result for an input file without the *Expressions* section.

### Constants

The section with constants has the following syntax

    Constants
      name = constant expression,
      name = constant expression,
      ...
      name = constant expression;

All constant definitions must be separated by comma and the last definition must end with a semicolon. It is possible to refer to previously defined constants in constant definitions. All constants are assumed to be real rational numbers. It is not possible to define irrational constants (e.g., a square root of 2). Irrational constants may be defined in the *Definitions* section.

Examples:

    Constants
      k = 0.1,
      c = 1 / k * 2;

### Variables

The section with variables has the following syntax

    Variables
      type var_name in [low, high],
      ...
      type var_name in [low, high];

All variable definitions must be separated by comma and the last definition must end with a semicolon. Each variable has a type, name, and the interval of values. The type can be omitted; in this case, a variable will get the `real` type.

Variables can be of the following types:

- `real`: the type of real values. This type is assigned by default to variables without explicit type.
- `float16`: the type of IEEE-754 16-bit floating-point numbers (half precision).
- `float32`: the type of IEEE-754 32-bit floating-point numbers (single precision).
- `float64`: the type of IEEE-754 64-bit floating-point numbers (double precision).
- `float128`: the type of IEEE-754 128-bit floating-point numbers (quadruple precision).

Names of variables should be different from names of constants.

All variables must be bounded. Bounds are given as constant expressions.

Examples:

    Constants
      k = 2;

    Variables
      float32 x in [0, 1],
      y in [sqrt(2), 20/7 + 0.1],
      real z in [-3, 4.5 + k]; // k is a constant

In the example above, `y` and `z` are real variables, `x` is a single precision variable.

It is also possible to define a variable which has uncertainties. It is done in the following way

    type var_name in [low, high] +/- uncertainty

Here, `uncertainty` must be a positive constant expression. Suppose that `x` is a variable in the interval `[a, b]` with uncertainty `u`. Then we have the following relation between actual value (`val(x)`) of `x` and its uncertain value: `|val(x) - x| <= u` and `a <= val(x) <= b`. Note that the actual value of `x` is bounded by `a` and `b` (not by `a - u` and `b + u`).


### Definitions

The section with definitions has the following syntax

    Definitions
      def_name = expr,
      ...
      def_name = expr;

All definitions must be separated by comma and the last definition must end with a semicolon. It is possible to refer to previous definitions. Definition names must be different from names of variables and constants.

Examples:

    Definitions
      t = x * y + 1,
      z = (t - 1) / (t + 1);

There is an alternative way to write a definition

      def_name rnd= expr

Here, `rnd` must be one of rounding operators (see the *Rounding* section). The meaning of this definition is the following: the rounding operator `rnd` is recursively applied to subexpressions of the right hand side `expr`. There is one restriction: `rnd` is not applied to another rounding operator and it does not propagate inside another rounding operator.

Examples:

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


### Expressions

The section with expressions has the following syntax

    Expressions
      expr_name = expr,
      ...
      expr_name = expr;

All expressions must be separated by comma and the last expression must end with a semicolon. This section works in the same way as the *Definitions* section but names of expressions (and the equality sign) can be omitted. Expressions without names will get automatically generated names "Expression k" where k is the number of the expression in the definition list. 

It is also possible to use the alternative syntax of definitions with automatic rounding (see the *Definitions* section).

Examples:

    Expressions
      2 * x,	// This expression will be named "Expression 1"
      t rnd16= 2 * x,
      1 + t;   // This expression will be named "Expression 3"

In the example above, the value of the last expression is `1 + rnd16(rnd16(2) * rnd16(x))`.

## Rounding