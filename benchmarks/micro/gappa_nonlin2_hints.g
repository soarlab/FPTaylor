@rnd = float<ieee_64,ne>;

x = rnd(x_);
y = rnd(y_);

t rnd= x * y;
r rnd= (t - 1) / (t * t - 1);

Mt = x * y;
Mr = (Mt - 1) / (Mt * Mt - 1);

{ x in [1.001, 2] /\ y in [1.001, 2] -> |r - Mr| in ? /\ r in ?}
(Mt - 1) / (Mt * Mt - 1) -> 1 / (Mt + 1);
$ x, y in 100;

