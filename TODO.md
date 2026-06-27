# Compiler
- change bytecode to support larger integer sizes: 1b for instruction, 8b for integers (check big endian?)
- possibly support multi-precision numbers on C Interpreter <=> Integer type in Haskell

# Interpreter
- add division and error handling with monads (how to foreign import monad from C?)

# Parser
- ignore spaces
- multiple operators
- add operator precendence
- add fancy parse errors (use Either monad?)

# Repl
- use Haskeline for convenience?
