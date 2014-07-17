@rnd = float<ieee_64,ne>;

t = rnd(t_);

r rnd= t / (t + 1);
Mr = t / (t + 1);

{ t in [0,999]
    -> |r - Mr| in ? /\ r in ? }


