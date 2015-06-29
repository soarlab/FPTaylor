@rnd = float<ieee_64,ne>;

x = rnd(x_);

half = rnd(5.00000000000000000000e-01);
S1  = rnd(-1.66666666666666324348e-01);
S2  = rnd(8.33333333332248946124e-03);
S3  = rnd(-1.98412698298579493134e-04);
S4  = rnd(2.75573137070700676789e-06);
S5  = rnd(-2.50507602534068634195e-08);
S6  = rnd(1.58969099521155010221e-10);

z rnd= x * x;
w rnd= z * z;
r rnd= S2+z*(S3+z*S4) + z*w*(S5+z*S6);
v rnd= z*x;
k_sin1 rnd= x+v*(S1+z*r);

Mz = x * x;
Mw = Mz * Mz;
Mr = S2+Mz*(S3+Mz*S4) + Mz*Mw*(S5+Mz*S6);
Mv = Mz*x;
Mk_sin1 = x+Mv*(S1+Mz*Mr);

{ x in [-0.785, 0.785]
    -> |k_sin1 - Mk_sin1| in ? }

#$ x in 100;
