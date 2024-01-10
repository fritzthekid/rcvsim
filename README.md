# rcvsim - RISC-V Simulator in Erlang

**The project is for the sole purpose as to learn Erlang and RISC-V.**

## Idea

In order to learn Erlang programming language and since Erlang supports messages passing,
I figured it's good to should hardware simulation. Since I do not know next to nothing about
RISC-V, but am eager to learn, I figured, the best starting point is to simulate its behavior.

I chose only the RV32I. The target is to simulate the behavior given the assembler code of
a function.

## Build

If you want to try, there are some test c-functions in csrc:

~~~
$ git clone ...
$ cd rcvsim
$ make
$ rebar3 compile
$ rebar3 eunit
~~~

Or you may compile your own c-code to assembler and try to run it from shell:

~~~
$ riscv64-unknown-elf-gcc -S -g -O2 -march=rv32imac -mabi=ilp32 yourfunc.c
$ rebar3 shell
1> rvsmain:run("yourfunc.s")
~~~

Probably your function is not working correctly, 
since pretty many instructions are still missing.

## Currently supported

Only one function, no arguments, only way use it can be seen in the small c-functions,
in csrc.
