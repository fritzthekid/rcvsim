all: simple-func.o func-with-globals.o copy

CCOPTIONS= -g -O0 -save-temps -march=rv32imac -mabi=ilp32

simple-func.o: simple-func.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} simple-func.c

func-with-globals.o: func-with-globals.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} func-with-globals.c

copy: simple-func.s func-with-globals.s
	cp *.s ../data/
