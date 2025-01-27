# Introduction to rcvsim - RISC-V Simulator in Erlang
## Overview

rcvsim is a RISC-V Simulator written in Erlang. It provides functionalities to simulate the execution of RISC-V assembly code, allowing users to test and verify their assembly programs in a controlled environment.

## Features

-    Erlang-based Simulator: Utilizes the Erlang programming language for concurrent and robust simulation.
-    Support for RISC-V Instructions: Capable of simulating a wide range of RISC-V instructions.
-    Memory and Register Management: Includes modules for handling memory and registers during simulation.
-    Assembly Code Processing: Reads and processes RISC-V assembly files, extracting labels, code, and global variables.
-    Extensible and Configurable: Designed to be extensible and configurable to meet various simulation needs.

## Getting Started

## To get started with rcvsim, follow these steps:

    Clone the repository:

~~~
git clone https://github.com/fritzthekid/rcvsim.git
~~~

Navigate to the project directory:
~~~
cd rcvsim
~~~

Compile the Erlang modules:

~~~
erlc src/*.erl
~~~

Run the simulator with a sample program:
~~~
    erl -noshell -s rvsmain run -s init stop
~~~

## Modules

- rvsmain: Main entry point and core functionalities for the simulator.
- rvsmemory: Handles memory management for the simulation.
- rvsops: Provides functions for disassembling RISC-V instructions.
- rvsreadasm: Reads and processes RISC-V assembly files.

## Example Usage

Here is an example of how to use rcvsim to simulate a simple RISC-V program:

-    Write a RISC-V assembly program and save it as program.s.
-    Run the simulator with the assembly program:
    
~~~
    erl -noshell -s rvsmain run program.s -s init stop
~~~

## Contributing

Contributions to rcvsim are welcome! Feel free to open issues or submit pull requests on the GitHub repository.
