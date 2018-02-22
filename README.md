# mini-C

This project is part of the course **[INF564 - Compilation](https://www.enseignement.polytechnique.fr/informatique/INF564/)**, very well taught at École Polytechnique by Jean-Christophe Filliâtre with assistance of Mário Pereira. It consists of a compiler of a small fragment of the C programming language called mini-C. It is only meant to help us learn the fundamental principles of the compilation to x86-64 and illustrates very well the ideas behind Xavier Leroy's CompCert (C compiler programmed in Caml and programmed and proved in Coq).

The teaching staff (Jean-Christophe Filliâtre and Mário Pereira) programmed a big part of the project, including the parser and interpreters for both RTL and ERTL. Here is a list of the modules I wrote:

 * Typing: Type system.
 * Ops: Instruction selection x86-64.
 * Rtl: Translation from Ops to RTL.
 * Ertl: Translation from RTL to ERTL.
 * Kildall: Life time of variables using Kildall's algorithm.