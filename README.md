A fairly advanced compiler framework with a complete front-end for the latest standard of the C programming language.  
Uses a modular and extensible architecture build around a virtual-machine model with an intermediate representation in SSA form running inside an interpreter. 

Developed during college and used as my B.sc. dissertation project (available below, Romanian language only).  
[B.sc. Dissertation PDF](https://github.com/user-attachments/files/17976987/gratian_lup_bsc_thesis.pdf) (130 pages)  
[Compiler presentation PDF](https://github.com/user-attachments/files/17977054/gratian_lup_compiler_presentation.pdf) (28 slides)  

Implements more than 30 inter- and intra-procedural optimizations and analyses (global value numbering and PRE, redundant load/store elimination, expression re-association, conditional constant propagation, peephole simplifications, aggressive dead code elimination, loop-invariant code motion, jump-threading, inlining, indirect call promotion, interprocedural constant propagation, symbolic and type-based alias analysis, operand interval estimation, call graph construction and many more).
