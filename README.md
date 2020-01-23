# Interval Analysis by Abstract Interpretation
Project for the Static Analysis subject for my MSc. Formal Methods in Computer Science

The project is written in Ciao, the main file is while.pl

To run the project just load while.pl it into a ciao interpreter:

 use_module('/home/..../src/while.pl').

and type:

 run(TokenList,AST,CFG,CFGSize,CFGHead,CFGEnds,_,Fixpoint,NumIterations,_,FixpointWithWidening,NumIterationsWithWidening).

This will analyze a preloaded while program, you can write your own programs in the test.pl file and edit the run predicate in while.pl to run them.