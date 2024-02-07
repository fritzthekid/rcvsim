#ifndef __STRING_H__
#define __STRING_H__
/* only dummy file */
//int32_t strcpy(char dest[], char source[]);
static char *rvsstrcpy(char dest[], const char source[]) {
  int32_t i;
  for (i=0; source[i] != 0 && i < 100; i++) {
    dest[i] = source[i];
  }
  dest[i] = (char) 0;
  return dest;
}

static inline int32_t rvsstrlen(const char *str) {
  int32_t i;
  for (i=0; str[i] != 0 && i < 100; i++);
  return i;
}
#endif
