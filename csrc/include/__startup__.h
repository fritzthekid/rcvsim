/** 
 * RCVSIM starup file, it includes: 
 * - system environment
 * - seting stackpointer (sp) at position 200
 * - int32_t argc
 * - int32_t argv[400], address 400
 * - jump to loader
 * - loader, which finally calls main and exit regularily
 */

#ifndef __NOTRISC__
#include <stdint-gcc.h>
asm (".text\n.start:\n\t"
     "li sp,3000\n\t"
     "j  loader\n\t");

extern int32_t sysargs[100];
asm (".text\n\t"
     ".globl\tsysargs\n\t"
     ".addr\tsysargs, 4000\n\t");

int32_t main(int32_t argc, char **argv);

void __attribute__ ((noinline)) loader(void) {
  asm ("lui a5,%hi(sysargs)\n\t"
       "lw a0,%lo(sysargs)(a5)\n\t"
       "lui a1,%hi(sysargs+4)\n\t"
       "addi sp,sp,-16\n\t"
       "addi a1,a1,%lo(sysargs+4)\n\t"
       "sw ra,12(sp)\n\t"
       "call main\n\t"
       "exit\n\t");
}
#endif
