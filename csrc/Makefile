CC=riscv64-unknown-elf-gcc
SRCS = $(wildcard *.c)
INCS = $(wildcard include/*.h)
ASDIR=../_build/obj
ASFILES=$(patsubst %.c,$(ASDIR)/%.s,$(SRCS))

all: $(ASFILES)

INCLUDES= -Iinclude
CCOPTIONS= -g -O2 -march=rv32imac -mabi=ilp32

$(ASDIR)/%.s: %.c ${INCS} Makefile
	$(CC) -S ${CCOPTIONS} ${INCLUDES} -o $@ $<

main: main.c test2.c
	gcc -o main -D'__NOTRISCV__' main.c test2.c

