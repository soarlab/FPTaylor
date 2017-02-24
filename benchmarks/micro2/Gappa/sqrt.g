@rnd = float<ieee_32,ne>;

x = rnd(x_);

sqrt_sub rnd= sqrt(x + 1) - sqrt(x);
M_sqrt_sub = sqrt(x + 1) - sqrt(x);

sqrt_add rnd= 1 / (sqrt(x + 1) + sqrt(x));
M_sqrt_add = 1 / (sqrt(x + 1) + sqrt(x));

{ x in [1, 1000]
    -> |sqrt_sub - M_sqrt_sub| / M_sqrt_sub in ? /\ 
       |sqrt_sub - M_sqrt_sub| in ? /\
       |sqrt_add - M_sqrt_add| in ? /\
       |sqrt_add - M_sqrt_add| / M_sqrt_add in ? }


$ x in 1000;

