@rnd = float<ieee_64,ne>;

x1 = rnd(Mx1);
x2 = rnd(Mx2);
x3 = rnd(Mx3);
x4 = rnd(Mx4);
x5 = rnd(Mx5);
x6 = rnd(Mx6);

r1 rnd=  x1 * x4 * (-x1 + x2 + x3 - x4 + x5 + x6)
                 + x2 * x5 * (x1 - x2 + x3 + x4 - x5 + x6)
		 + x3 * x6 * (x1 + x2 - x3 + x4 + x5 - x6)
		 - x2 * x3 * x4 - x1 * x3 * x5
		 - x1 * x2 * x6 - x4 * x5 * x6;  

Mr1 = Mx1 * Mx4 * (-Mx1 + Mx2 + Mx3 - Mx4 + Mx5 + Mx6)                 + Mx2 * Mx5 * (Mx1 - Mx2 + Mx3 + Mx4 - Mx5 + Mx6)		 + Mx3 * Mx6 * (Mx1 + Mx2 - Mx3 + Mx4 + Mx5 - Mx6)		 - Mx2 * Mx3 * Mx4 - Mx1 * Mx3 * Mx5		 - Mx1 * Mx2 * Mx6 - Mx4 * Mx5 * Mx6;  

{ 
  Mx1 in [4, 6.36] /\
  Mx2 in [4, 6.36] /\
  Mx3 in [4, 6.36] /\
  Mx4 in [4, 6.36] /\
  Mx5 in [4, 6.36] /\
  Mx6 in [4, 6.36] 
    -> |r1 - Mr1| in ?}


