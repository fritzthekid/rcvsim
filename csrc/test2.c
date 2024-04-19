#include <stdint-gcc.h>
#include <stdlib.h>

#ifndef WITHMAIN
#include "__startup__.h"
#endif

extern int32_t buffer[];

int32_t load_add_store();

#ifndef WITHMAIN
asm (".text\n\t"
     ".globl\tbuffer\n\t"
     ".addr\tbuffer, 500\n\t");
#endif

/* #ifndef WITHMAIN   */
/* void __attribute__ ((noinline)) loader(void) { */
/*   asm ("li sp,100\n\t" */
/*        "li s0,200\n\t" */
/*        "li a0,400\n\t" */
/*        "li a1,2\n\t" */
/*        "sw a1,0(a0)\n\t" */
/*        "li a1,0\n\t" */
/*        "sw a1,4(a0)\n\t" */
/*        "load a1,5\n\t" */
/*        "sw a1,8(a0)"); */
/*   load_add_store(); */
/*   asm ("nop\n\t" */
/*        "exit\n\t"); */
/* } */
/* #endif */

int32_t load_add_store() {
  int32_t a,b,c;
  a = buffer[0];
  b = buffer[1];
  c = buffer[2];
  for (int32_t i=0; i<c; i++) {
    a = (a*a) % 1001;
    buffer[10+i] = a;
  }
  buffer[3] = a + b;
  return a+b;
}

int32_t main(int32_t argc, char **argv) {
  buffer[0] = strtol(argv[0]);
  buffer[1] = strtol(argv[1]);
  buffer[2] = strtol(argv[2]);
  load_add_store();
  return 0;
}
