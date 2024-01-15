#include <stdint-gcc.h>

extern int32_t buffer[];

#ifndef WITHMAIN  
asm (".text\n.start:\n\t"
     "nop\n\t"
     "j  loader\n\t");
#endif

int32_t load_add_store();

#ifndef WITHMAIN  
void __attribute__ ((noinline)) loader(void) {
  asm ("load sp,100\n\t"
       "load s0,200\n\t"
       "load a0,400\n\t"
       "load a1,17\n\t"
       "sw a1,0(a0)\n\t"
       "load a1,14\n\t"
       "sw a1,4(a0)");
  load_add_store();
  asm ("nop\n\t"
       "exit\n\t");
}
#endif

int32_t load_add_store() {
  int32_t a,b;
  a = buffer[0];
  b = buffer[1];
  buffer[2] = a + b;
  return 0;
}
