output="""
bounds: [-inf, inf]

Computing absolute errors
-1: exp = -53: 8.290480e-06 (low = 8.290480e-06, subopt = 0.0%)

Solving the exact optimization problem
exact bound (exp = -53): 3.359666e+03 (low = 1.261620e+02, subopt = 96.2%)
total2: 9.204282e-22 (low = 9.204282e-22, subopt = 0.0%)
exact total: 3.729979e-13 (low = 1.400680e-14, subopt = 96.2%)

Elapsed time: 1.46955
*************************************

-------------------------------------------------------------------------------
Problem: sum

Optimization lower bounds for error models:
The absolute error model (exact): 1.776356e-15 (0x1p-49) (suboptimality = 20.0%)

Bounds (without rounding): [-inf, +inf]

Absolute error (exact): 2.220447e-15 (0x1.4p-49)

Elapsed time: 2.19

-------------------------------------------------------------------------------
Problem: nonlin1

Optimization lower bounds for error models:
The absolute error model (exact): 1.662719e-16 (0x1.7f659db6ba873p-53) (suboptimality = 0.3%)

Bounds (without rounding): [-inf, +inf]

Absolute error (exact): 1.666951e-16 (0x1.805f63f0d55f7p-53)

Elapsed time: 1.16

-------------------------------------------------------------------------------
Problem: nonlin2

Optimization lower bounds for error models:
The absolute error model (exact): 1.400679e-14 (0x1.f8a5ea37c757cp-47) (suboptimality = 96.2%)

Bounds (without rounding): [-inf, +inf]

Absolute error (exact): 3.729979e-13 (0x1.a3f551ffb2c7ap-42)

Elapsed time: 1.47
"""

from tasks import fptaylor

r = fptaylor.FPTaylorResults(output)
print(r)