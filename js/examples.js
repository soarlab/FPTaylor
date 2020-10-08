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
  r rnd64=     x1 + ((2*x1*(t/(x1*x1 + 1))*
    (t/(x1*x1 + 1) - 3) + x1*x1*(4*(t/(x1*x1 + 1))-6))*
    (x1*x1 + 1) + 3*x1*x1*(t/(x1*x1 + 1)) + x1*x1*x1 + x1 +
    3*((3*x1*x1 + 2*x2 -x1)/(x1*x1 + 1)));

Expressions
  jet = r;
`
  },
  {
    name: 'doppler1',
    data: `
Variables
  real u in [-100, 100];
  real v in [20, 20000];
  real T in [-30, 50];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  

Expressions
  doppler1 = r;
`
  },
  {
    name: 'doppler2',
    data: `
Variables
  real u in [-125, 125];
  real v in [15, 25000];
  real T in [-40, 60];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  
Expressions
  doppler2 = r;
`
  },
  {
    name: 'doppler3',
    data: `
Variables
  real u in [-30, 120];
  real v in [320, 20300];
  real T in [-50, 30];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  

Expressions
  doppler3 = r;
`
  },
  {
    name: 'doppler3',
    data: `
Variables
  real u in [-30, 120];
  real v in [320, 20300];
  real T in [-50, 30];

Definitions
  t1 rnd64= 331.4 + 0.6 * T;
  r rnd64= (-t1 * v) / ((t1 + u) * (t1 + u));
  

Expressions
  doppler3 = r;
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
];