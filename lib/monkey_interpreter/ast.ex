defmodule MonkeyInterpreter.Ast do
  alias MonkeyInterpreter.Ast

  defmodule Identifier do
    @type t :: String.t()
  end

  # TODO not implemented yet
  defmodule Expression do
    @type t :: %__MODULE__{}
    @enforce_keys []
    defstruct @enforce_keys
  end

  # A program consists of a series of statements.
  defmodule Program do
    @type t :: %__MODULE__{statements: list(Ast.Statement.t())}
    @enforce_keys [:statements]
    defstruct @enforce_keys
  end

  # We're using tagged tuples instead of interfaces/protocols. A Statement could be any of these kinds of statements.
  defmodule Statement do
    @type t ::
            {:let, Ast.LetStatement.t()}
            | {:return, Ast.ReturnStatement.t()}
  end

  # Each let statement has the name/identifier, and the value. The literal is used for debugging only.
  defmodule LetStatement do
    @type t :: %__MODULE__{
            literal: String.t(),
            name: Ast.Identifier.t(),
            value: Ast.Expression.t()
          }
    @enforce_keys [:literal, :name, :value]
    defstruct @enforce_keys
  end

  # Each return statement has the expression value being returned. The literal is used for debugging only.
  defmodule ReturnStatement do
    @type t :: %__MODULE__{literal: String.t(), return_value: Ast.Expression.t()}
    @enforce_keys [:literal, :return_value]
    defstruct @enforce_keys
  end
end
