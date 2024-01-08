#include <stdint-gcc.h>

//int32_t buffer[1000];

int32_t simple_func(int x, int y) { //, int32_t *out) {
  int32_t a = 0;
  asm ("load sp,100\n\t"
       "load s0,200\n\t"
       "load a0,400\n\t"
       "load a1,17\n\t"
       "sw a1,0(a0)\n\t"
       "load a1,14\n\t"
       "sw a1,4(a0)");
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
    
