/**
 * test4 is about tail recursion
 * three subroutines call each other with "tail call subroutine non-returnable"
 * the inner algorithm solves factional algotihm.
 */

#include <stdint-gcc.h>
#include <stdlib.h>

#ifdef __NOTRISCV__
#include <stdio.h>
#else
#include "__startup__.h"
#endif

int32_t __attribute__ ((noinline)) factorial(int32_t c, int32_t res) {
  if (c < 1) return res;
  return factorial(c-1,c*res);
}

int32_t __attribute__ ((noinline)) do_somenumber(int32_t number) {
  return factorial(number-2,1);
}

int32_t __attribute__ ((noinline)) somenumber(int32_t number) {
  int32_t val = number+2;
  return do_somenumber(val);
}


int32_t main(int32_t argc, char **argv) {
  if ( argc < 1 ) return 0;
  return somenumber(strtol(argv[0]));
}
