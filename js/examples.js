const configExamples = [
  {
    name: 'Improved Rounding',
    data: `fp-power2-model = true
opt-exact = true
opt-approx = false
print-precision = 7
verbosity = 1
`
  },
  {
    name: 'Standard Rounding',
    data: `fp-power2-model = false
opt-exact = true
opt-approx = false
print-precision = 7
verbosity = 1
`
  },
  {
    name: 'Relative Error',
    data: `fp-power2-model = true
opt-exact = true
opt-approx = false
rel-error = true
abs-error = false
print-precision = 7
verbosity = 1
`
  },
];

const inputExamples = [
  {
    name: 'Basic',
    data: `Variables
  real x in [0, 20];

Expressions
  r rnd64= x + x;
`
  },
  {
    name: 'jet',
    data: `Variables
  real x1 in [-5, 5];
  real x2 in [-20, 5];

Definitions
  t rnd64= (3*x1*x1 + 2*x2 - x1);
  r rnd64= x1 + ((2*x1*(t / (x1*x1 + 1)) *
    (t / (x1*x1 + 1) - 3) + x1*x1*(4*(t / (x1*x1 + 1)) - 6)) *
    (x1*x1 + 1) + 3*x1*x1*(t / (x1*x1 + 1)) + x1*x1*x1 + x1 +
    3 * ((3*x1*x1 + 2*x2 - x1) / (x1*x1 + 1)));

Expressions
  jet = r;
`
  },
  {
    name: 'doppler',
    data: `
{
Variables
  real u in [-100, 100];
  real v in [20, 20000];
  real T in [-30, 50];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  

Expressions
  doppler1 = r;
}
{
Variables
  real u in [-125, 125];
  real v in [15, 25000];
  real T in [-40, 60];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  
Expressions
  doppler2 = r;
}
{
  Variables
  real u in [-30, 120];
  real v in [320, 20300];
  real T in [-50, 30];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  
Expressions
  doppler3 = r;
}
`
  },
  {
    name: 'sine',
    data: `
Variables
  real x in [-1.57079632679, 1.57079632679];

Expressions
  sine rnd64= x - (x*x*x)/6.0 + (x*x*x*x*x)/120.0 - (x*x*x*x*x*x*x)/5040.0;
`
  },
  {
    name: 'hartman3',
    data: `
Variables
    real x1 in [0, 1];
    real x2 in [0, 1];
    real x3 in [0, 1];
  
  Definitions
    e1 rnd64= 3.0 * ((x1 - 0.3689) * (x1 - 0.3689)) + 10.0 * ((x2 - 0.117) * (x2 - 0.117))
              + 30.0 * ((x3 - 0.2673) * (x3 - 0.2673));
    e2 rnd64= 0.1 * ((x1 - 0.4699) * (x1 - 0.4699)) + 10.0 * ((x2 - 0.4387) * (x2 - 0.4387))
              + 35.0 * ((x3 - 0.747) * (x3 - 0.747));
    e3 rnd64= 3.0 * ((x1 - 0.1091) * (x1 - 0.1091)) + 10.0 * ((x2 - 0.8732) * (x2 - 0.8732))
              + 30.0 * ((x3 - 0.5547) * (x3 - 0.5547));
    e4 rnd64= 0.1 * ((x1 - 0.03815) * (x1 - 0.03815)) + 10.0 * ((x2 - 0.5743) * (x2 - 0.5743))
              + 35.0 * ((x3 - 0.8828) * (x3 - 0.8828));
    exp1 = rnd[64, ne, 1.5](exp(-e1));
    exp2 = rnd[64, ne, 1.5](exp(-e2));
    exp3 = rnd[64, ne, 1.5](exp(-e3));
    exp4 = rnd[64, ne, 1.5](exp(-e4));
  
  Expressions
    hartman3 rnd64= -(1.0 * exp1 + 1.2 * exp2 + 3.0 * exp3 + 3.2 * exp4);
`
  },
  {
    name: 'himmilbeau',
    data: `
Variables
  real x1 in [-5, 5];
  real x2 in [-5, 5];

Expressions
  himmilbeau rnd64= (x1*x1 + x2 - 11)* (x1*x1 + x2 - 11) + (x1 + x2*x2 - 7)*(x1 + x2*x2 - 7);
`
  },
  {
    name: 'kepler',
    data: `
{
Variables
  real x1 in [4, 6.36];
  real x2 in [4, 6.36];
  real x3 in [4, 6.36];
  real x4 in [4, 6.36];
  real x5 in [4, 6.36];
  real x6 in [4, 6.36];

Expressions
  kepler0 rnd64= x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6
                 + x1 * (-x1 + x2 + x3 - x4 + x5 + x6);
  kepler1 rnd64= x1 * x4 * (-x1 + x2 + x3 - x4)
                 + x2 * (x1 - x2 + x3 + x4)
                 + x3 * (x1 + x2 - x3 + x4)
                 - x2 * x3 * x4 - x1 * x3 - x1 * x2 - x4;
  kepler2 rnd64= x1 * x4 * (-x1 + x2 + x3 - x4 + x5 + x6)
                 + x2 * x5 * (x1 - x2 + x3 + x4 - x5 + x6)
                 + x3 * x6 * (x1 + x2 - x3 + x4 + x5 - x6)
                 - x2 * x3 * x4 - x1 * x3 * x5
                 - x1 * x2 * x6 - x4 * x5 * x6;
}
`
  },
  {
    name: 'logexp',
    data: `
Variables
  real x in [-8, 8];

Definitions
  f rnd64= x;
  e = rnd[64, ne, 1.5](exp(f));
  t rnd64= 1 + e;
  r = rnd[64, ne, 1.5](log(t));

Expressions
  logexp = r;
`
  },
  {
    name: 'predatorPrey',
    data: `
Constants
  r = 4.0;
  K = 1.11;

Variables
  real x in [0.1, 0.3];
	
Definitions
  res rnd64= (r*x*x) / (1 + (x/K)*(x/K));
  
Expressions
  predatorPrey = res;
`
  },
  {
    name: 'rigidBody',
    data: `
Variables
  real x1 in [-15, 15];
  real x2 in [-15, 15];
  real x3 in [-15, 15];

Definitions
  r1 rnd64= -x1*x2 - 2*x2*x3 - x1 - x3;
  r2 rnd64= 2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2;

Expressions
  rigidBody1 = r1;
  rigidBody2 = r2;
`
  },
  {
    name: 'sineOrder3',
    data: `
Variables
  real z in [-2, 2];

Expressions
  sineOrder3 rnd64= 0.954929658551372 * z - 0.12900613773279798 * (z*z*z);
`
  },
  {
    name: 'sqroot',
    data: `
Variables
  real y in [0, 1];
  
Expressions
  sqroot rnd64= 1.0 + 0.5*y - 0.125*y*y + 0.0625*y*y*y - 0.0390625*y*y*y*y;
`
  },
  {
    name: 'turbine',
    data: `
Variables
  real v in [-4.5, -0.3];
  real w in [0.4, 0.9];
  real r in [3.8, 7.8];

Definitions
  r1 rnd64= 3 + 2 / (r * r) - 0.125 * (3 - 2 * v) * (w * w * r * r) / (1 - v) - 4.5;
  r2 rnd64= 6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5;
  r3 rnd64= 3 - 2/(r*r) - 0.125 * (1+2*v) * (w*w*r*r) / (1-v) - 0.5;
  
Expressions
  turbine1 = r1;
  turbine2 = r2;
  turbine3 = r3;
`
  },
  {
    name: 'verhulst',
    data: `
Constants
  r = 4.0;
  K = 1.11;

Variables
  real x in [0.1, 0.3];

Definitions
  res rnd64= (r*x) / (1 + (x/K));
  

Expressions
  verhulst = res;
`
  },
  {
    name: 'carbonGas',
    data: `
Constants
  k = 1.3806503e-23;

Variables
  T in [300.0, 300.0];
  a in [0.401, 0.401];
  b in [42.7e-6, 42.7e-6];
  N in [1000, 1000];
  p in [3.5e7, 3.5e7];
  V in [0.1, 0.5];
	
Definitions
  res rnd64= (p + a * (N / V) * (N / V)) * (V - N * b) - k * N * T;
  

Expressions
  carbon_gas = res;
`
  }
];