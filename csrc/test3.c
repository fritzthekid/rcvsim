#include <stdint-gcc.h>
#ifdef WITHMAIN
#include <sdtio.h>
#endif

#ifndef WITHMAIN  
asm (".text\n.start:\n\t"
     "li sp,200\n\t"
     "j  loader\n\t");
#endif

int32_t main(void);

#ifndef WITHMAIN  
void __attribute__ ((noinline)) loader(void) {
  asm ("li s0,100\n\t"
       "li a20,400\n\t"
       "li a21,17\n\t"
       "sw a21,0(a20)\n\t"
       "li a21,14\n\t"
       "sw a21,4(a20)\n\t"
       "li a21,5\n\t"
       "sw a21,8(a20)\n\t");
  main();
  asm ("nop\n\t"
       "exit\n\t");
}
#endif
  
int32_t buffer[4000];

int32_t __attribute__ ((noinline)) myfunc(int32_t e, int32_t f, int32_t g, int32_t out[]) {
  out[0] = (e * f) + g;
  return e+f+g;
}

int32_t __attribute__ ((noinline)) main(void) {
  int32_t retval;
  retval = myfunc(buffer[0],buffer[1],buffer[2],&buffer[3]);
  retval = retval*retval;
#ifdef WITHMAIN
  printf("",buffer[0],buffer[1],buffer[2],buffer[3]);
#endif
  return retval;
}
