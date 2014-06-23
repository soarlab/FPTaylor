@rnd = float<ieee_64,ne>;


t1 rnd= 331.4 + 0.6 * T;
r rnd= (-t1 * v) / ((t1 + u) * (t1 + u));
	
Mt1 = 331.4 + 0.6 * T;
Mr = (-Mt1 * v) / ((Mt1 + u) * (Mt1 + u));

{ u in [-30, 120] /\
  v in [320, 20300] /\
  T in [-50, 30]
    -> |r - Mr| in ? }


