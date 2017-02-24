int main(void)
{
  double x, y;
  double res;

  x = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-4.0, 4.0);
  y = __BUILTIN_DAED_DBETWEEN_WITH_ULP(-4.0, 4.0);
  
  if (x > y) {
    res = 0.238604 - 0.143624*x + 0.0137*x*x + 0.143624*y + 0.00605411*x*y + 0.0137*y*y;
  } else {
    res = 0.238604 + 0.143624*x + 0.0137*x*x - 0.143624*y + 0.00605411*x*y + 0.0137*y*y;
  }

  DSENSITIVITY(res);

  return 0;
}
