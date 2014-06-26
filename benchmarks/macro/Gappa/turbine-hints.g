@rnd = float<ieee_64,ne>;

v = rnd(Mv);
w = rnd(Mw);
r = rnd(Mr);

r1 rnd= 3 + 2 / (r * r) - 0.125 * (3 - 2 * v) * (w * w * r * r) / (1 - v) - 4.5;
#r2 rnd= 6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5;
#r3 rnd= 3 - 2 / (r * r) - 0.125 * (1 + 2 * v) * (w * w * r * r) / (1 - v) - 0.5;

Mr1 = 3 + 2 / (Mr * Mr) - 0.125 * (3 - 2 * Mv) * (Mw * Mw * Mr * Mr) / (1 - Mv) - 4.5;
#Mr2 = 6*Mv - 0.5 * Mv * (Mw*Mw*Mr*Mr) / (1-Mv) - 2.5;
#Mr3 = 3 - 2 / (Mr * Mr) - 0.125 * (1 + 2 * Mv) * (Mw * Mw * Mr * Mr) / (1 - Mv) - 0.5;

{ Mv in [-4.5, -0.3] /\
  Mw in [0.4, 0.9] /\
  Mr in [3.8, 7.8] 
    -> |r1 - Mr1| in ? }
#       |r2 - Mr2| in ? /\
#       |r3 - Mr3| in ? }

$ Mv, Mw, Mr in 10;
$ Mv in 100;
$ Mw in 100;
$ Mr in 100;
