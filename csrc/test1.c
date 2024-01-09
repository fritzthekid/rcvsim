#include <stdint-gcc.h>

extern int32_t buffer[];

int32_t load_add_store() {
  int32_t a,b,c;
  asm ("load sp,100\n\t"
       "load s0,200\n\t"
       "load a0,400\n\t"
       "load a1,17\n\t"
       "sw a1,0(a0)\n\t"
       "load a1,14\n\t"
       "sw a1,4(a0)\n\t"
       "load a1,3\n\t"
       "sw a1,8(a0)");
  a = buffer[0];
  b = buffer[1];
  c = buffer[2];
  a += ((a*c) >> 1);
  buffer[3] = a + b;
  return 0;
}
    
