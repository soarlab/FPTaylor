int main(void)
{
  double u, v, T;
  double t1;
  double r;
  u = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-30.0, 120.0);
  v = __BUILTIN_DAED_DBETWEEN_WITH_ULP(320.0, 20300.0);
  T = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-50.0, 30.0);

  t1 = 331.4 + 0.6 * T;
  r = (-t1 * v) / ((t1 + u) * (t1 + u));
  DSENSITIVITY(r);

  return 0;
}
