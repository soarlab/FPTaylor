int main(void)
{
  double t, r;

  t = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.0, 999.0);

  r = t / (t + 1);
  DSENSITIVITY(r);
}
