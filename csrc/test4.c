/**
 * test4 is about recursion and rudimentary I/O
 * two algotihm are part of the program
 * - factorial numbers
 * and
 * - fibonacci numbers 
 * 
 * main(int32_t argc, int32_t *argv) not standard c:
 * reason not datatype other than int32_t is supported and library functions for char handling 
 */

#include <stdint-gcc.h>
#include <stdlib.h>

#ifdef __NOTRISCV__
#include <stdio.h>
#else
#include "__startup__.h"
#endif

extern int32_t buffer[];
#ifndef WITHMAIN
asm (".text\n\t"
     ".globl\tbuffer\n\t"
     ".addr\tbuffer, 5000\n\t");
#endif

int32_t fibonacci(int32_t number) {
  // Von 0 bis 2 entspricht die Fibonacci-Zahl der Eingabe
  if (number < 0) {
    return 0;
  } else if (number < 2) {
    return number;
  }
  return fibonacci(number - 1) + fibonacci(number - 2);
}

int32_t __attribute__ ((noinline)) factorial(int32_t c, int32_t res) {
  if ( c > 1 ) {
    return factorial(c-1,c*res);
  } else if (c < 1) {
    return 0;
  }
  return res;
}

int32_t  main(int32_t argc, char **argv) {
  int32_t fac,fib;
  fac=0;fib=0;
  if (argc <= 0) return 0;
  fac = factorial(strtol(argv[0]),1);
  if (argc > 1) {
    fib = fibonacci(strtol(argv[1]));
  }
  return fib+fac;
}

