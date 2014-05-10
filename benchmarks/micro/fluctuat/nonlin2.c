int main(void)
{
  double x, y, t, r;
  x = DBETWEEN(1.001, 2.0);
  y = DBETWEEN(1.001, 2.0);
  t = x * y;
  r = (t - 1.0) / (t * t - 1.0);
  DSENSITIVITY(r);
}
