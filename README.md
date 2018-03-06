# mini-C

This project is part of the course **[INF564 - Compilation](https://www.enseignement.polytechnique.fr/informatique/INF564/)**, very well taught at École Polytechnique by Jean-Christophe Filliâtre with assistance of Mário Pereira. It consists of implementing a compiler of a small fragment of the C programming language called mini-C. Its purpose is to help us learn the fundamental principles behind compilation to x86-64 and, more specifically, those from Xavier Leroy's CompCert (C compiler programmed in Caml and programmed and proved in Coq).

The teaching staff (Jean-Christophe Filliâtre and Mário Pereira) programmed a big part of the project, including the parser, interpreters for RTL, ERTL and LTL (extremely hepful for debugging) and the module X86_64, which contains numerous well written tools for producing x86_64 assembly code. Here is a list of the modules I wrote (in the order they are used by the compiler):

 * Typing: Type system.
 * Ops: Instruction selection x86-64.
 * Rtl: Translation from Ops to RTL.
 * Ertl: Translation from RTL to ERTL.
 * Kildall: Life time of variables using Kildall's algorithm.
 * Interference: Construction of the interference graph.
 * Coloring: Register allocation using George-Appel's algorithm.
 * Ltl: Translation from ERTL to LTL.
 * Assembly: Translation from LTL to x86_64 Assembly.
