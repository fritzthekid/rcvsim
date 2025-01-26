CC=riscv64-unknown-elf-gcc
SRCS = $(wildcard *.c)
INCS = $(wildcard include/*.h)
ASDIR=../_build/obj
ASFILES=$(patsubst %.c,$(ASDIR)/%.s,$(SRCS))
OBJFILES=$(ASDIR)/test2.o
EXEFILES=$(ASDIR)/test2
# $(patsubst %.c,$(ASDIR)/%.o,$(SRCS))

all: $(ASFILES) $(OBJFILES) # $(EXEFILES)

INCLUDES= -Iinclude
CCOPTIONS= -O2 -march=rv32imac -mabi=ilp32

$(ASDIR)/%.s: %.c ${INCS} Makefile
	$(CC) -S -g ${CCOPTIONS} ${INCLUDES} -o $@ $<

$(ASDIR)/%.o: %.c ${INCS} Makefile
	$(CC) -c ${CCOPTIONS} ${INCLUDES} -DWITHMAIN -o $@ $<

#$(ASDIR)/%: %.c ${INCS} Makefile
#	$(CC) -static ${CCOPTIONS} ${INCLUDES} -DWITHMAIN -o $@ $<

main: main.c test2.c
	gcc -o main -D'__NOTRISCV__' main.c test2.c

