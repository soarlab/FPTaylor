@rnd = float<ieee_64,ne>;

x = rnd(Mx);

sqrt_sub rnd= sqrt(x + 1) - sqrt(x);
M_sqrt_sub = sqrt(Mx + 1) - sqrt(Mx);

{ Mx in [1, 1000]
    -> |(sqrt_sub - M_sqrt_sub) / M_sqrt_sub| in ? }

$ x in 2000;
