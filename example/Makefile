all: simple-func.o

CCOPTIONS= -g -save-temps -march=rv32imac -mabi=ilp32
simple-func.o: simple-func.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} simple-func.c

