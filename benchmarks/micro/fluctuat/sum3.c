int main(void)
{
  double x0, x1, x2, p0, p1, p2, sum;
  x0 = DBETWEEN(1.0, 2.0);
  x1 = DBETWEEN(1.0, 2.0);
  x2 = DBETWEEN(1.0, 2.0);
  
  p0 = (x0 + x1) - x2;
  p1 = (x1 + x2) - x0;
  p2 = (x2 + x0) - x1;
  
  sum = (p0 + p1) + p2;
  DSENSITIVITY(sum);
}
