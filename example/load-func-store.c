#include <stdint-gcc.h>

extern int32_t buffer[];

int32_t load_func_store() {
  int32_t a,b,c,d,e;
  a = buffer[0];
  b = buffer[1];
  c = a+b;
  d = c * 15;
  e = d >> 2;
  buffer[2] = c;
  buffer[3] = d;
  buffer[4] = e;
  return 0;
}

