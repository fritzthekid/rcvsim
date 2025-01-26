#ifndef __RVSLIB_H__
#define __RVSLIB_H__

char rvs_sendmessage_extern(const char *, int32_t);

char rvs_sendmessage(const char *msg, int32_t len) {
  asm (".text\n\t"
       ".globl\trvs_strtol_extern\n\t"
       ".type\trvs_sendmessage_extern, @extern_function\n\t");
  char ret = rvs_sendmessage_extern(msg,len);
  asm ( "\n\t" );
  return ret;
}

/*
 * from here: https://stackoverflow.com/questions/5558492/divide-by-10-using-bit-shifts 
 */
static inline uint32_t divu10(uint32_t n) {
    uint32_t q, r;
    q = (n >> 1) + (n >> 2);
    q = q + (q >> 4);
    q = q + (q >> 8);
    q = q + (q >> 16);
    q = q >> 3;
    r = n - (((q << 2) + q) << 1);
    return q + (r > 9);
}

uint32_t rvs_itoa(uint32_t val, char buf[], int32_t cur) {
  if ( val <= 0 || cur < 0) {
    buf[cur] = 'x';
    buf[cur-1] = '0';
    buf[10] = 0;
    return cur-1;
  }
  int32_t lval = val % 16;
  if ( lval < 10 ) {
    buf[cur] = (char) (lval+48);
    rvs_itoa(val >> 4,buf,cur-1);
  } else {
    buf[cur] = (char) (lval+55);
    rvs_itoa(val >> 4 ,buf,cur-1);
  }  
}

#endif
