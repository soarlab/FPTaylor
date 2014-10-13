from z3 import *

def find_upper_bound(f, constraints, u, timeout):
    s = Solver()
    if timeout > 0:
        s.set("timeout", timeout)
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

  
def maximize(f, constraints, lb, ub, tol, timeout):
    s = Solver()
    if timeout > 0:
        s.set("timeout", timeout)
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
    

def find_bounds(f, constraints, tol, timeout):
    ub = find_upper_bound(f, constraints, 0, timeout)
    lb = -find_upper_bound(-f, constraints, 0, timeout)
    u1, u2 = maximize(f, constraints, lb, ub, tol, timeout)
    l1, l2 = maximize(-f, constraints, -ub, -lb, tol, timeout)
    return -l2, u2
