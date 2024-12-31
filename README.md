 # Book
 This repo is my attempt to follow along with the book "Writing An Interpreter In Go" by Thorsten Ball, but implemented in Elixir. The book can be found at: https://interpreterbook.com

 # Status
 The implementation is complete and almost exactly follows the interpreter behavior in the book. The code is obviously structured differently because Elixir is functional. Also, there are some other differences in the internals due to Elixir/Golang specifics.

 # TODOs
 - Figure out why the dialyzer is not warning when we explicitly use incorrect atoms in TokenType.t().
 - Remove all the "literal" fields in the AST nodes, since I'm not using them like the book does in the tests.
 - The lexer and parser both need better error handling (it just crashes the REPL process right now). Implement error handling similar to how I did in the evaluator.
 - A handful of remaining cleanup/style TODOs sprinkled throughout the codebase.
