Kostis Seiras
1115201800174

  Si     p  l   i
A Simple ProLog Interpreter

-- Build

chmod +x build.sh
./build.sh

-- Run

./sipli

-- Commands

"load file" <PATH>    : Load rules of <PATH>.
"?" <Query>           : Execute <Query> using Top Down evaluation. Queries should be '.' terminated.
"v^" <Query> <N>      : Execute <Query> using Bottom Up evaluation. <N> is an optional argument, the interpreter will output the results of the first N evaluation rounds.
"unify" <Term> <Term> : Try to unify the given terms.
":q"                  : Exit the interpreter.

After executing a Top Down evaluation query, you have to press either ";", if you want to see whether there are more solutions, or "." to continue.



