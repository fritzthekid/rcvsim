#include <stdint-gcc.h>

extern int32_t buffer[];

int32_t load_func_store() {
  int32_t a,b,c,d,e;
  asm ("load sp,100\n\t"
       "load s0,200\n\t"
       "load a0,400\n\t"
       "load a1,17\n\t"
       "sw a1,0(a0)\n\t"
       "load a1,14\n\t"
       "sw a1,4(a0)");
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

