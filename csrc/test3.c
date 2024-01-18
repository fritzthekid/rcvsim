#include <stdint-gcc.h>
#ifdef __NOTRISCV__
#include <sdtio.h>
#else
#include "__startup__.h"
#endif
  
/* extern int32_t buffer[]; */
/* #ifndef __NOTRISCV__ */
/* asm (".text\n\t" */
/*      ".globl\tbuffer\n\t" */
/*      ".addr\tbuffer, 500\n\t"); */
/* #endif */

int32_t __attribute__ ((noinline)) myfunc(int32_t e, int32_t f, int32_t g, int32_t *out) {
  out[0] = (e * f) + g;
  return (e*f)+g;
}

int32_t __attribute__ ((noinline)) main(int32_t argc, int32_t *argv) {
  int32_t retval;
  retval = myfunc(argv[0],argv[1],argv[2],&argv[3]);
  retval = retval*retval;
#ifdef WITHMAIN
  printf("",argv[0],argv[1],argv[2],argv[3]);
#endif
  return retval;
}
