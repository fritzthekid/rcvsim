CC=riscv64-unknown-elf-gcc
SRCS = $(wildcard *.c)
ASDIR=../test/assembler
ASFILES=$(patsubst %.c,$(ASDIR)/%.s,$(SRCS))

all: $(ASFILES)

CCOPTIONS= -g -O2 -march=rv32imac -mabi=ilp32

$(ASDIR)/%.s: %.c Makefile
	$(CC) -S ${CCOPTIONS} -o $@ $<
