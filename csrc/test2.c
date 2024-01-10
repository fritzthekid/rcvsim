#include <stdint-gcc.h>

extern int32_t buffer[];

int32_t load_add_store() {
  int32_t a,b,c;
#ifndef WITHMAIN  
  asm ("li sp,100\n\t"
       "li s0,200\n\t"
       "li a0,400\n\t"
       "li a1,2\n\t"
       "sw a1,0(a0)\n\t"
       "li a1,0\n\t"
       "sw a1,4(a0)\n\t"
       "load a1,5\n\t"
       "sw a1,8(a0)");
#endif
  a = buffer[0];
  b = buffer[1];
  c = buffer[2];
  for (int32_t i=0; i<c; i++) {
    a = (a*a) % 1001;
    buffer[10+i] = a;
  }
  buffer[3] = a + b;
  return 0;
}
    
