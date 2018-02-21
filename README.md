# mini-C

This project is part of the course INF564 - Compilation, very well taught at École Polytechnique by Jean-Christophe Filliâtre with assistance of Mário Pereira. It consists of a compiler of a small fragment of the C programming langage called mini-C. It is only meant to help us learn the fundamental principles of the compilation to x86-64 and illustrates very well the ideas behind Xavier Leroy's CompCert (C compiler programmed in Caml and programmed and proved in Coq).

To help the students, the professor (Jean-Christophe Filliâtre) programmed a big part of the project, including the parser and interpreters for RTL and ERTL. Here is a list of the modules written by me:

 * Typing: Type system.

 * Ops: Instruction selection x86-64
 
 * Rtl: Translation from Ops to Rtl

 * Ertl: Translation from Rtl to Ertl

 * Kildall: Life time of variables using Kildall's algorithm