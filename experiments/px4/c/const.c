#include <stdio.h>
#include <math.h>

void main()
{
  float _accel_range_scale = 9.80665f / 4096;
  float _accel_scale_x_offset = 0;
  float _accel_scale_x_scale = 1;
  float sample_freq = 1000;
  float _cutoff_freq = 30;
  #define M_PI_F 3.14159f

  float fr = sample_freq / _cutoff_freq;
  float ohm = tanf(M_PI_F / fr);
  float cs = cosf(M_PI_F / 4.0);
  float c = 1.0 + 2.0 * cs * ohm + ohm * ohm;
  float _b0 = ohm * ohm / c;
  float _b1 = 2.0 * _b0;
  float _b2 = _b0;
  float _a1 = 2.0 * (ohm * ohm - 1.0) / c;
  float _a2 = (1.0 - 2.0 * cs * ohm + ohm * ohm) / c;

  printf("_b0 = %a\n_b1 = %a\n_b2 = %a\n_a1 = %a\n_a2 = %a\n\n", _b0, _b1, _b2, _a1, _a2);
}

