all: simple-func.o copy

CCOPTIONS= -g -O0 -save-temps -march=rv32imac -mabi=ilp32
simple-func.o: simple-func.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} simple-func.c

copy: simple-func.s
	cp simple-func.s ../data/
