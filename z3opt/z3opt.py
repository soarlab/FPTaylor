try:
    from z3 import *
except ImportError as e:
    msg = """Python: no module named z3. 
             Provide this path with the --z3-python-lib option:

             ./fptaylor --z3-python-lib="path to Z3 python" ...

             (Note: use a full home directory path instead of ~)

             Or add this path to the PYTHONPATH environment variable."""
    raise ImportError(msg)

def z3_abs(x):
    return If(x >= 0, x, -x)

def z3_max(x, y):
    return If(x >= y, x, y)

def z3_min(x, y):
    return If(x <= y, x, y)

def find_upper_bound(f, constraints, u, timeout, seed):
    s = Solver()
    if seed >= 0:
        s.set("seed", seed)
    if timeout > 0:
        s.set("timeout", timeout)
#        s.set("solver2_timeout", timeout)
    while (1):
#        print u
        s.reset()
        s.add(constraints)
        s.add(f > u)
        r = s.check()
#        print r
        if r == sat:
            if u < 10:
                u = 10
            else:
                u = 2 * u
        else:
            return u

def maximize(f, constraints, lb, ub, tol, timeout, seed):
    s = Solver()
    if seed >= 0:
        s.set("seed", seed)
    if timeout > 0:
        s.set("timeout", timeout)
#        s.set("solver2_timeout", timeout)
#    s.add(constraints)
    while (ub - lb > tol):
        m = (ub + lb) / 2.0
#        print m, lb, ub
        s.reset()
        s.add(constraints)
        s.add(f > m)
#        s.push()
#        s.add(f > m)
        r = s.check()
#        print r
#        s.pop()
        if r == sat:
            lb = m
        elif r == unsat:
            ub = m
        else:
            return lb, ub
    return lb, ub
    

def find_bounds(f, constraints, tol, timeout, seed):
    ub = find_upper_bound(f, constraints, 0, 0, seed)
    lb = -find_upper_bound(-f, constraints, 0, 0, seed)
    u1, u2 = maximize(f, constraints, lb, ub, tol, timeout, seed)
    l1, l2 = maximize(-f, constraints, -ub, -lb, tol, timeout, seed)
    return -l2, u2
