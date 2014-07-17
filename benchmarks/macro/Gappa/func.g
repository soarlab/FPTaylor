@rnd = float<ieee_64,ne>;

x = rnd(Mx);
y = rnd(My);
z = rnd(Mz);

sine rnd= x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
sqroot rnd= 1.0 + 0.5*y - rnd(0.125)*y*y + rnd(0.0625)*y*y*y - rnd(0.0390625)*y*y*y*y;
sineOrder3 rnd= rnd(0.954929658551372) * z -  rnd(0.12900613773279798)*(z*z*z);

Msine= Mx - (Mx*Mx*Mx)/6.0 + (Mx*Mx*Mx*Mx*Mx)/120.0 - (Mx*Mx*Mx*Mx*Mx*Mx*Mx)/5040.0;
Msqroot = 1.0 + 0.5*My - 0.125*My*My + 0.0625*My*My*My - 0.0390625*My*My*My*My;
MsineOrder3 = 0.954929658551372 * Mz -  0.12900613773279798*(Mz*Mz*Mz);

{ Mx in [-1.57079632679, 1.57079632679] /\
  My in [0, 1] /\
  Mz in [-2, 2]
    -> |sine - Msine| in ? /\
       |sqroot - Msqroot| in ? /\
       |sineOrder3 - MsineOrder3| in ? }
