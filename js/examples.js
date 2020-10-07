const configExamples = [
  {
    name: 'Improved Rounding',
    data: `fp-power2-model = true
opt-exact = true
opt-approx = false
`
  },
  {
    name: 'Standard Rounding',
    data: `fp-power2-model = false
opt-exact = true
opt-approx = false
`
  }
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
  }
];