@rnd = float<ieee_32,ne>;

f = rnd(f_);

Lg1 = 0xaaaaaa.0p-24;
Lg2 = 0xccce13.0p-25;
Lg3 = 0x91e9ee.0p-25;
Lg4 = 0xf89e26.0p-26;

s rnd= f/(2.0+f);
z rnd= s*s;
w rnd= z*z;
t1 rnd= w*(Lg2+w*Lg4);
t2 rnd= z*(Lg1+w*Lg3);
R rnd= t2+t1;
hfsq rnd= 0.5*f*f;
k_log1pf rnd= s*(hfsq+R);

Ms = f/(2.0+f);
Mz = Ms*Ms;
Mw = Mz*Mz;
Mt1 = Mw*(Lg2+Mw*Lg4);
Mt2 = Mz*(Lg1+Mw*Lg3);
MR = Mt2+Mt1;
Mhfsq = 0.5*f*f;
Mk_log1pf = Ms*(Mhfsq+MR);

{ f in [-0.1, 0.1]
    -> |k_log1pf - Mk_log1pf| in ? }

$ f in 100;
