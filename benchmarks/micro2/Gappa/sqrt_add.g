@rnd = float<ieee_64,ne>;

x = rnd(Mx);

sqrt_add rnd= 1 / (sqrt(x + 1) + sqrt(x));
M_sqrt_add = 1 / (sqrt(Mx + 1) + sqrt(Mx));

{ Mx in [1, 1000]
    -> |sqrt_add - M_sqrt_add| in ? /\
       |(sqrt_add - M_sqrt_add) / M_sqrt_add| in ? }


