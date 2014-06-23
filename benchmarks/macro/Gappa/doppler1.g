@rnd = float<ieee_64,ne>;

t1 rnd= 331.4 + 0.6 * T;
r rnd= (-t1 * v) / ((t1 + u) * (t1 + u));
	
Mt1 = 331.4 + 0.6 * T;
Mr = (-Mt1 * v) / ((Mt1 + u) * (Mt1 + u));

{ u in [-100, 100] /\
  v in [20, 20000] /\
  T in [-30, 50]
    -> |r - Mr| in ? }


