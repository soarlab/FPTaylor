int main(void)
{
  double t, r;
  t = DBETWEEN(0.0, 999.0);
  r = t / (t + 1);
  DSENSITIVITY(r);
}
