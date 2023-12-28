#include <stdint-gcc.h>

extern int32_t buffer[];

int32_t load_add_store() {
  int32_t a,b;
  a = buffer[0];
  b = buffer[1];
  buffer[2] = a + b;
  return 0;
}
    
