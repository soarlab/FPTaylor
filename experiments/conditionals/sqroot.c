int main(void)
{
  double a, b;
  double res;

  a = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.0, 1.0);
  b = __BUILTIN_DAED_DBETWEEN_WITH_ULP(0.0, 1.0);

  if (b - a >= 0.5) {
    res = -b / (b - a);
  }
  else {
    res = -b / 0.5;
  }

  DSENSITIVITY(res);

  return 0;
}
