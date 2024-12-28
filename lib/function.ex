defmodule MonkeyInterpreter.Function do
  alias MonkeyInterpreter.{Ast, Environment}

  # This is the object representation of a function/closure, not the same as FunctionLiteral, which is an AST node.
  @type t :: %__MODULE__{
          parameters: list(Ast.Identifier.t()),
          body: Ast.BlockStatement.t(),
          environment: Environment.t()
        }
  @enforce_keys [:parameters, :body, :environment]
  defstruct @enforce_keys
end
