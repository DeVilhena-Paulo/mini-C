# mini-C

This project is part of the course **[INF564 - Compilation](https://www.enseignement.polytechnique.fr/informatique/INF564/)**, taught at École Polytechnique by Jean-Christophe Filliâtre with assistance of Mário Pereira, both to whom I'm very grateful. It consists of the implementation a compiler of a small fragment of the C programming language, called mini-C. The purpose of the project is to help us learn the fundamental principles behind compilation to x86-64 and, more specifically, those from Xavier Leroy's CompCert (C compiler programmed in Caml and programmed and proved in Coq).

## Usage

### Prerequisites

 * OCaml version 4.05 or later
 * GNU Make
 * A 64-bit machine

After getting a local copy of the project, you can build it by running the following command on the console from within the project's folder:
```sh
$ make
```
You will then have the compiler read to run. Let's try it !
Move to the folder called tests:
```sh
$ cd tests
```
Then compile one of its mini-C source files to x86-64 assembly with your brand new mini-C compiler. For example:
```sh
$ .././mini-c mandelbrot.c
```
Finally, you can use the assembler (as) to create the object file and the linker (ld) to build the executable. Or, more simply, just use gcc:
```sh
$ gcc mandelbrot.s -o mandelbrot && ./mandelbrot
```

## Understanding the Project

The teaching staff (Jean-Christophe Filliâtre and Mário Pereira) programmed a big part of the project, including the parser, interpreters for RTL, ERTL and LTL (extremely helpful for debugging) and the module X86_64, which contains numerous well written tools for producing x86_64 assembly code.

The work I have done was essentially to take an abstract syntax tree as input and produce the corresponding x86_64 assembly code. This task was divided into several steps, each of which targeting a particular problem. In order to explain these steps, I will describe the modules involved on this process as they are used by the compiler.

### Typing Module

We start by verifying either the program is sound or not, that is, if the program is composed of well typed instructions, expressions and definitions according to a predefined set of rules. Typing one of these will update a global environment which is then used to type next constructions of the language.

If the program pass the type check, an assembly code version of him is guaranteed and any errors during the nexts steps is a compiler's fault. Of course, that does not guarantee the correctness of the program. If the program does not pass this step, it's probably a fault of the programmer, who will be soon comforted with an explanatory error message enabling him to continue in the practice of mini-C.

### Ops Module

In this module, we get near of the assembly code by a bit. The main structure of the program is preserved, but expressions are translated into a language much more similar to assembly in form. Indeed, the most important implementations in this module are the _smart constructurs_. Other useful information that we extract at this step are the local variables of a function, the variables that are hidden at each new block, the size of a structure and the position of its fields.

### Rtl Module

We translate Ops to Rtl (Register Transfer Language) at this step. Two main ideas are comprised by this language: the introduction of registers for memory management and the replacement of the abstract syntax tree by a __control flow graph__. These registers are purely abstract (pseudo-registers), but their importance come from the way they mimic the mov instructions from assembly, saving us from this work in the future.

### Ertl Module

Here, we give some final touches to our Rtl instructions before moving to register allocation. We make calling conventions explicit at three phases of the program: at the beginning of a function, where we make sure that all the values stored at callee saved registers aren't destroyed by its execution and we store the function parameters that were passed by the caller; at the ending of a function, where we restore the callee saved registers; and finally, at the moment of calling a function, where we need to move argument values to the right predefined parameter registers (%rdi, %rsi, %rdx, ...). This treatment explains the E in ERTL (Explicit Register Transfer Language).

### Kildall Module

This module is the beginning of our register allocation journey. In order to implement our pseudo registers as real registers of the machine, we need first to find out for how long and at each instructions the value at a given pseudo register is needed. That’s the purpose of this module, which implement Kildall’s algorithm to find the subset of pseudo-registers that **could** be needed by each instruction.

### Interference Module

After the life analysis is complete, this module will basically build a graph for each function of our program where the vertices are registers (pseudo or not) and the arcs are of two kinds: interference, expressing that the two ending vertices can't be allocated in the same real register, and preference, expressing that it would be preferable to allocate the two ending vertices in the same real register, but not required.

### Coloring Module

With the interference graph already built, we are finally able to replace our pseudo registers with one of the allocatable registers of the machine or with a location on the stack. The approach implemented is known as George and Appel iterated register coalescing algorithm.

### Ltl Module

The main work of register allocation having already been implemented in the three past modules, here we only make sure to respect some restrictions from the assembly x86_64 set of instructions such as a maximum of one memory access per operation.

### Assembly Module

The translation from LTL (Location Transfer Language) to assembly code is pretty straightforward. The only sensitive point is the if-else branch, where we try to minimize the total number of jump instructions. To produce the actual assembly text file we make use of a very well written library, the x86_64 module (already mentioned above).

## Additional Work

In addition to what have been requested, I made some other improvements just for learning purposes. I explain here some of the difficulties I’ve encountered during their implementation and eventual particularities of the code.

### Tail Call Optimization

Tail calls are noticed and optimized at the translation from RTL to ERTL. The price for this optimization is to solve the problem of replacing the caller’s frame with the frame of the function being called. The way it works on the project is as follows. First we compute the gap between actual and next frames in number of bytes (the number of parameters located on the stack for each function is what determines it), let’s call it __gap__. Then we save the values stored at __0(%rbp)__ and __8(%rbp)__ (previous __%rbp__ and return address) on temporary pseudo-registers. Finally, together with possible function parameters exceeding the 6 available real registers we can start the process of replacing frames by moving __0(%rbp)__ to __gap(%rbp)__, __8(%rbp)__ to __gap+8(%rbp)__, the last function parameter to __gap+16(%rbp)__ and so on.

Other small modifications were necessary, but the only one that needs clarification is how we make the life analysis of functions benefiting from a tail call. It’s not clear what are the pseudo registers being used and defined by a tail call, so a first solution is to save Kildall's output for each instruction alloc_frame and make use of it every time we see a tail call. Another solution is to guess the subset of registers used and defined by a tail call instruction. I’ve noticed that __use__ = __callee_saved__ U __parameter registers__ and __def__ = __caller_saved__ work for every test.

Note: To run the interpreters, you will have to comment the tail call treatment at the Ertl file and recompile the project.

### George and Appel Iterated Register Coalescing

Prior to a simpler algorithm, I implemented George and Appel iterated register coalescing in order to solve the coloring problem. The theory was very well explained in class and the code is easy to understand. No particular problems were found.
