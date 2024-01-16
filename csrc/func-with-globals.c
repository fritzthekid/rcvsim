#include <stdint-gcc.h>

int32_t buffer[1000];

#ifndef WITHMAIN
asm (".text\n\t"
     ".globl\tbuffer\n\t"
     ".addr\tbuffer, 400\n\t");
asm (".text\n.start:\n\t"
     "nop\n\t"
     "j  loader\n\t");
#endif

int32_t simple_func(int x, int y, int32_t *out);

#ifndef WITHMAIN  
void __attribute__ ((noinline)) loader(void) {
  asm ("load sp,100\n\t"
       "load s0,200\n\t"
       "load a0,400\n\t"
       "load a1,17\n\t"
       "sw a1,0(a0)\n\t"
       "load a1,14\n\t"
       "sw a1,4(a0)");
  simple_func(17,14,&buffer[0]);
  asm ("nop\n\t"
       "exit\n\t");
}
#endif

int32_t simple_func(int x, int y, int32_t *out) {
  int32_t a;
  a = 0;
  a = x;
  a += y*x;
  buffer[0] = 18;
  buffer[1] = a;
  a += 17*y;
  a = (16+a);
  a = a*y;
  *out = a;
  return 0;
}

    
