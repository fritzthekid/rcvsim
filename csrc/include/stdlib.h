/**
 * test5.c:
 * - local implementation of stdlib.h using build in lib functions
 */

#ifndef __STDLIB__H
#define __STDLIB__H

float atof(char *);
int32_t __attribute__ ((noinline)) rvs_strtol_extern(char *);
int32_t __attribute__ ((noinline)) strtol(char *str) {
  int32_t ret;
  asm (".text\n\t"
       ".globl\trvs_strtol_extern\n\t"
       ".type\trvs_strtol_extern, @extern_function\n\t");
  ret = rvs_strtol_extern(str);
  asm ( "nop\n\t" );
  return ret;
}

static inline int32_t atoi(char * arg) {
  return strtol(arg);
}
#endif
