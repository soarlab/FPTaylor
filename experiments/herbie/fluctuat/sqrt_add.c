#include <math.h>

int main(void)
{
  float x, r;
  x = DBETWEEN(1.0, 1000.0);
  r = sqrt(x + 1) - sqrt(x);
  DSENSITIVITY(r);
}
