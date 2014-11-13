int main(void)
{
  double v, w, r;
  double res;

  v = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-4.5, -0.3);
  w = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.4, 0.9);
  r = __BUILTIN_DAED_DBETWEEN_WITH_ULP(3.8, 7.8);

  res = 6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5;
  DSENSITIVITY(res);

  return 0;
}
