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
#ifdef __NOTRISCV__
#include <stdio.h>
#else
#include "__startup__.h"
#endif

extern int32_t buffer[];
#ifndef WITHMAIN
asm (".text\n\t"
     ".globl\tbuffer\n\t"
     ".addr\tbuffer, 500\n\t");
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

int32_t __attribute__ ((noinline)) main(int32_t argc, int32_t *argv) {
  int32_t fac,fib,com;
  fac=0;fib=0,com=0;
  if (argc <= 0) return 0;
  fac = factorial(argv[0],1);
  if (argc > 1) {
    fib = fibonacci(argv[1]);
  }
  if (argc>4) {
    com = compare(argv[2],argv[3],argv[4]);
  }
  return fib+fac+com;
}
