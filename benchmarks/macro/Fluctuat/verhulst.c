int main(void)
{
  double r, K, x;
  double res;

  r = __BUILTIN_DAED_DBETWEEN_WITH_ULP(4.0, 4.0);
  K = __BUILTIN_DAED_DBETWEEN_WITH_ULP(1.11, 1.11);
  x = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.1, 0.3);

  res = (r*x) / (1 + (x/K));
  DSENSITIVITY(res);

  return 0;
}
