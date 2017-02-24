@rnd = float<ieee_32,ne>;

x = rnd(Mx);

r rnd= x + 1.125;
Mr = x + 1.125;

{ x in [600, 1000] ->
    |r - Mr| in ?
}
