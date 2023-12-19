#include <stdint-gcc.h>

//int32_t buffer[1000];

int32_t simple_func(int x, int y) { //, int32_t *out) {
  int32_t a = 0;
  a = x;
  a += y*x;
  //  buffer[0] = 18;
  //buffer[1] = a;
  a += 17*y;
  a = (16+a);
  a = a*y;
  // *out = a;
  return a;
}
    
