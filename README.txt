-- Commands

"load file" <PATH>    : Load rules of <PATH>.
"?" <Query>           : Execute <Query> using Top Down evaluation.
"v^" <Query> <N>      : Execute <Query> using Bottom Up evaluation. <N> is an optional argument, the interpreter will output the results of the first N evaluation rounds.
"unify" <Term> <Term> : Try to unify the given terms.
":q"                  : Exit the interpreter.

After executing a Top Down evaluation query, you have to press either ";", if you want to see whether there are different solutions, or "." to continue.



