all: simple-func.o func-with-globals.o load-add-store.o copy

CCOPTIONS= -g -O0 -save-temps -march=rv32imac -mabi=ilp32

simple-func.o: simple-func.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} simple-func.c

load-add-store.o: load-add-store.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} load-add-store.c

func-with-globals.o: func-with-globals.c Makefile
	riscv64-unknown-elf-gcc -c ${CCOPTIONS} func-with-globals.c

copy: simple-func.s func-with-globals.s load-add-store.s
	cp *.s ../data/
