@rnd = float<ieee_64,ne>;

x0 = rnd(x0_);
x1 = rnd(x1_);
x2 = rnd(x2_);

p0 rnd= (x0 + x1) - x2;
p1 rnd= (x1 + x2) - x0;
p2 rnd= (x2 + x0) - x1;
sum rnd= (p0 + p1) + p2;

Mp0 = (x0 + x1) - x2;
Mp1 = (x1 + x2) - x0;
Mp2 = (x2 + x0) - x1;
Msum = (Mp0 + Mp1) + Mp2;

{ x0 in [1, 2] /\ x1 in [1, 2] /\ x2 in [1, 2]
    -> |sum - Msum| in ? /\ sum in ? }


