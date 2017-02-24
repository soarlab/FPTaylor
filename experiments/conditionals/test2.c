int main(void)
{
  double a, b;
  double res;

  a = __BUILTIN_DAED_DBETWEEN(0.0, 100.0);
  b = __BUILTIN_DAED_DBETWEEN(0.0, 100.0);

  if (b >= a) {
    res = b / (b - a + 0.5);
  }
  else {
    res = b / 0.5;
  }

  DSENSITIVITY(res);

  return 0;
}
