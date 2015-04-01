@rnd = float<ieee_64,ne>;

x = rnd(Mx);

r rnd= x / (x + 1);
Mr = Mx / (Mx + 1);

{ Mx in [1, 999]
    -> |r - Mr| in ? }

$ x in 1000;

