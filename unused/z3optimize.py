from z3 import *

[var_x0, var_x1] = Reals('var_x0, var_x1')

var_constraints = [var_x0 >= Q(-5,1), var_x0 <= Q(5,1), var_x1 >= Q(-5,1), var_x1 <= Q(5,1)]
constraints = []
f = (Q(1, 1) - (var_x0 * var_x1))

opt = Optimize()
opt.add(var_constraints + constraints)

result = opt.maximize(f)
opt.check()

print('max = {0}'.format(result.value()))
