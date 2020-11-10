const default_config = `
# All options of FPTaylor and their default values.
# Any line which starts with * or # is a comment line.

# All FPTaylor options are declared in this file.
# It is not necessary to edit this file to change options.
# A separate config file can be created with new values for some options
# and then it can be passed as a parameter to fptaylor:
#
# fptaylor -c config_file_name ...
#
# It is also possible to pass options as parameters in the following way:
# fptaylor --option_name value ...
#
# Some options have short names declared in this file as [short: id].
# An option with a short name can be used in the command line in the following way:
# fptaylor -id value ...
#
# A comment starting with ## gives a short description of the next defined option.
# \`fptaylor -help\` prints out all these short descriptions.

#**************************
# Basic options
#**************************

## If true then additional debug information is printed.
[short: d]
debug = true

## Specifies the verbosity level of the output.
# 0: print main results only
# 1: print important steps
# 2: print additional information for each step
# 3: print everything (including debug information)
[short: v]
verbosity = 2

## The output precision of all important values (final error bounds).
# The final error bounds are printed as correctly rounded decimal numbers
# with the \`print-precision\` number of digits.
print-precision = 7

## If true then lower bounds are printed for all optimization problems.
# If the difference between lower and upper bounds is significant
# (more than 10%) then it may be possible to increase the precision
# by changing optimization parameters (for instance, opt-x-abs-tol = 0).
print-opt-lower-bounds = true

## If true then exact error bound values are printed as hexadecimal floating-point numbers.
print-hex-floats = true

## If true then absolute round-off error is computed.
[short: abs]
abs-error = true

## If true then relative round-off error is computed.
[short: rel]
rel-error = false

## If true then ulp round-off error is computed
[short: ulp]
ulp-error = false

## The function range threshold for relative errors.
rel-error-threshold = 0.0001

## Type of variables without explicit types
default-var-type = float64

## Default rounding operation (for rounding operations without parameters)
default-rnd = rnd64

## If true then bounds of the analyzed expressions are computed.
# These bounds are always computed if rel-error = true.
find-bounds = false

## If true then uncertainty specifications for input variables are used.
uncertainty = false

## If true, all potential overflows and invalid operations throw an exception.
fail-on-exception = true

## If true then expressions are algebraically simplified before optimization.
# Maxima must be installed if maxima-simplification = true.
maxima-simplification = false

## If true then a more accurate rounding model is used.
# This model introduces discontinuous functions in optimization problems.
fp-power2-model = true

## If true then a special rounding model is used for real variables.
# This rounding model computes constant erros based on interval bounds of a variable.
# This model may yield better results when fp-power2-model = false.
const-approx-real-vars = false

## If true then second order error terms are estimated with an optimization procedure.
# This mode is slower but can yield more accurate results.
# It may be useful if the total second order error is too large.
intermediate-opt = false

# If true then rounding of equivalent expression will
# produce different indices (i.e., no error cancellation effect).
# May be useful to measure the error cancellation effect.
unique-indices = false

## If true then a proof certificate is created and saved.
proof-record = false

## Path to a directory where proof certificates are saved.
proof-dir = proofs

#**************************
# Logging options
#**************************

## Path to a log directory.
# Can be absolute or relative to a directory from which FPTaylor is called.
log-base-dir =

# Current date may be attached to log file names.
# This option specifies where date is attached to log file names.
# Possible values: start, end, none.
# Note: any value different from 'start' and 'end' has the same effect
# as 'none' (date is not attached).
# Log file names has the following format:
# start: [YYYY-MM-DD-hhmmss]_input_file_name.log
# end: input_file_name_[YYYY-MM-DD-hhmmss].log
# none: input_file_name.log
log-append-date = start

## Export all configuration options into the given file
export-options =

#**************************
# Temporary files
#**************************

## Path to a directory where temporary files are created.
tmp-base-dir =

## If true then subdirectories in the tmp directory are created.
# Each subdirectory has current date as its name.
tmp-date = false

## If defined then a C file for the search_mpfr(mpfi) tool (from ErrorBounds) is created.
# The following templates are supported:
# {task} is replaced with task's name.
export-error-bounds =

## If defined then a C file for the data_mpfi tool (from ErrorBounds) is created.
# The following templates are supported:
# {task} is replaced with task's name.
export-error-bounds-data =

#**************************
# Optimization options
#**************************

## Optimization method.
# Possible values:
# bb (basic interval branch and bound; OCaml compiler is required)
# bb-eval (basic interval branch and bound which does not require
#          OCaml compiler to be installed; slower than bb)
# z3 (z3 SMT solver with binary search)
# nlopt (nlopt optimization library; not rigorous)
# gelpia (GELPIA tool)
opt = bb-eval

## If true then an approximate optimization problem is solved.
[short: approx]
opt-approx = false

## If true then a full optimization problem is solved.
# In general, this problem is harder than the approximate optimization problem
# but may yield a better result.
[short: exact]
opt-exact = true

# Parameters below may be not supported by all optimization methods.

## Optimization relative tolerance for a function.
opt-f-rel-tol = 0.01

## Optimization absolute tolerance for a function.
opt-f-abs-tol = 0.01

## Optimization relative tolerance for a domain.
# If the initial infinite norm of a domain is R then
# subdomains will be subdivided only if
# ||subdomain||_inf > R * opt-x-rel-tol + opt-f-abs-tol.
opt-x-rel-tol = 0.0

## Optimization absolute tolerance for a domain.
opt-x-abs-tol = 0.01

## Optimization maximum number of iterations.
# Negative numbers denote unlimited iterations.
opt-max-iters = 1000000

## Optimization timeout (in milliseconds; should be integer).
opt-timeout = 10000

#**************************
# z3 options
#**************************

## If true then interval bounds are used as the first approximation for the Z3 backend.
z3-interval-bounds = true

## Path to the Z3 installation (use a full home directory path instead of ~).
# This path will be added to the LD_LIBRARY_PATH environment variable
# before invoking Z3.
# This path can be empty if it is already added to required environment
# variables.
z3-bin = 

## Path to the Z3 python library (use a full home directory path instead of ~).
# This path will be added to the PYTHONPATH environment variable
# before invoking Z3.
# This path can be empty if it is already added to required environment
# variables.
z3-python-lib = 

## Python command for Z3.
z3-python-cmd = python

## Z3 random seed
z3-seed = 0

#**************************
# bb options
#**************************

bb-compile = {base}/b_and_b/compile.sh {base} {input} {out}

# bb algorithm (possible values: opt0)
bb-alg = opt0

#**************************
# Gelpia options
#**************************

# In order to use Gelpia, set the GELPIA_PATH environment variable
# or copy Gelpia to the FPTaylor root directory.

#**************************
# nlopt options
#**************************

nlopt-cc = gcc -std=c99 -O3

nlopt-lib = -lnlopt -lm


#**************************
# Extra options
#**************************

develop = false
`
