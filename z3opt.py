from z3 import *

def find_upper_bound(f, constraints, u):
    s = Solver()
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

  
def maximize(f, constraints, lb, ub, tol):
    s = Solver()
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
        else:
            ub = m
    return lb, ub
    

def find_bounds(f, constraints, tol):
    ub = find_upper_bound(f, constraints, 0)
    lb = -find_upper_bound(-f, constraints, 0)
    u1, u2 = maximize(f, constraints, lb, ub, tol)
    l1, l2 = maximize(-f, constraints, -ub, -lb, tol)
    return -l2, u2
