int main(void)
{
  double x1, x2, x3;
  double r;

  x1 = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-15.0, 15.0);
  x2 = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-15.0, 15.0);
  x3 = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-15.0, 15.0);

  r = 2*x1*x2*x3 + 3*x3*x3 - x2*x1*x2*x3 + 3*x3*x3 - x2;
  DSENSITIVITY(r);

  return 0;
}
