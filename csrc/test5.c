/**
 * test4 is about tail recursion
 * three subroutines call each other with "tail call subroutine non-returnable"
 * the inner algorithm solves factional algotihm.
 */

#include <stdint-gcc.h>
#include <stdlib.h>
#include <string.h>

#ifdef __NOTRISCV__
#include <stdio.h>
#else
#include "__startup__.h"
#endif

extern char buffer[];

#ifndef WITHMAIN
asm (".text\n\t"
     ".globl\tbuffer\n\t"
     ".addr\tbuffer, 5000\n\t");
#endif

int32_t main(int32_t argc, char **argv) {
  if ( argc < 2 ) return 0;
  rvsstrcpy(buffer,argv[0]);
  rvsstrcpy(&buffer[rvsstrlen(argv[0])],argv[1]);
  return rvsstrlen(buffer);
}
