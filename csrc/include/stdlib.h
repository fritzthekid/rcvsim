/**
 * test5.c:
 * - local implementation of stdlib.h using build in lib functions
 */

#ifndef __STDLIB__H
#define __STDLIB__H

float atof(char *);
int32_t strtol(char *); //, char *, int);

#ifndef WITHMAIN
int32_t __attribute__ ((noinline)) rvs_strtol_extern(char *);
int32_t __attribute__ ((noinline)) strtol(char *str) {
  int32_t ret;
  asm (".text\n\t"
       ".globl\trvs_strtol_extern\n\t"
       ".type\trvs_strtol_extern, @extern_function\n\t");
  ret = rvs_strtol_extern(str);
  asm ( "\n\t" );
  return ret;
}
#endif

static inline int32_t atoi(char * arg) {
  return strtol(arg); //, NULL, 10);
}
#endif
