CC=riscv64-unknown-elf-gcc
SRCS = $(wildcard *.c)
ASDIR=../_build/obj
ASFILES=$(patsubst %.c,$(ASDIR)/%.s,$(SRCS))
INCLUDES=__startup__.h

all: $(ASFILES)

CCOPTIONS= -g -O2 -march=rv32imac -mabi=ilp32

$(ASDIR)/%.s: %.c $(INCLUDES) Makefile
	$(CC) -S ${CCOPTIONS} -o $@ $<

main: main.c test2.c
	gcc -o main -D'__NOTRISCV__' main.c test2.c

