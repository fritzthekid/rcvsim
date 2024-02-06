#include <stdint-gcc.h>
#include <stdlib.h>

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

int32_t main(int32_t argc, char **argv) {
  int32_t retval;
  int32_t out;
  retval = myfunc(strtol(argv[0]),strtol(argv[1]),strtol(argv[2]),out);
  retval = retval*retval;
  return retval;
}
