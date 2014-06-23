@rnd = float<ieee_64,ne>;

sine rnd= x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
sqroot rnd= 1.0 + 0.5*y - 0.125*y*y + 0.0625*y*y*y - 0.0390625*y*y*y*y;
sineOrder3 rnd= 0.954929658551372 * z -  0.12900613773279798*(z*z*z);

Msine= x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
Msqroot = 1.0 + 0.5*y - 0.125*y*y + 0.0625*y*y*y - 0.0390625*y*y*y*y;
MsineOrder3 = 0.954929658551372 * z -  0.12900613773279798*(z*z*z);

{ x in [-1.57079632679, 1.57079632679] /\
  y in [0, 1] /\
  z in [-2, 2]
    -> |sine - Msine| in ? /\
       |sqroot - Msqroot| in ? /\
       |sineOrder3 - MsineOrder3| in ? }

