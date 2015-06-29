@rnd = float<ieee_64,ne>;
@rnd32 = float<ieee_32,ne>;

x2 = rnd(Mx);
x = rnd32(Mx);

t rnd32= x + 1;
r rnd= x2 / t;

Mr = Mx / (Mx + 1);

{ Mx in [1.0, 999.0]
    -> |r - Mr| in ? /\
       |(r - Mr) / Mr| in ? }

