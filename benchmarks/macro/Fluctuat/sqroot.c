int main(void)
{
  double x;
  double res;

  x = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.0, 10.0);

  res = 1.0 + 0.5*x - 0.125*x*x + 0.0625*x*x*x - 0.0390625*x*x*x*x;
  DSENSITIVITY(res);

  return 0;
}
