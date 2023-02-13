Call-Pattern Analysis and Improving Lazy Non-Deterministic Computations
=======================================================================

This package contains an initial prototype
for analyzing call patterns in Curry programs.
This analysis is described in this paper:

Michael Hanus:
[Call Pattern Analysis for Functional Logic Programs](https://doi.org/10.1145/1389449.1389459),
Proc. of the 10th International ACM SIGPLAN Conference on
Principles and Practice of Declarative Programming (PPDP'08),
ACM Press, pp. 67-78, 2008

Moreover, the package contains also a transformation tool to improve
lazy non-deterministic computations by a demand analysis,
which is described in this paper:

Michael Hanus:
[Improving Lazy Non-Deterministic Computations by Demand Analysis](http://dx.doi.org/10.4230/LIPIcs.ICLP.2012.130),
Technical Communications of the 28th International Conference
on Logic Programming (ICLP 2012),
Leibniz International Proceedings in Informatics (LIPIcs),
vol. 17, pp. 130-143, 2012

After installing this package, use the command `curry-ndopt`
to analyze and transform your program.


Remarks:

* The implementation contains an analysis with depth-bounded terms.
  The default depth bound is 2. It can be changed to <k> by executing
  the command `curry-ndopt -d <k> prog`.

* Currently, the main program must contain all rules, i.e.,
  no modules are imported for the program analysis.
  In particular, primitive functions must be also defined
  in the program file (compare `benchmarks_callpattern/readfile.curry`).

* In programs containing higher-order applications
  (compare `benchmarks_callpattern/mapadddouble.curry`), rules to define
  an higher-order application operator `apply` are implicitly
  generated.


Some modules in the directory `src`:

Analysis.curry:
  The main analysis implementation.

TRS.curry:
  Some basic definitions to deal with term rewriting systems.

ReadFlatTRS.curry:
  An implementation of an I/O action to read a Curry program
  and return the corresponding rewrite rules.

OrCaseLifter.curry:
  A transformation on FlatCurry program that removes deeply nested
  case expressions by introducing auxiliary functions.
  Used by ReadFlatTRS.
