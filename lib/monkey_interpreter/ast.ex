defmodule MonkeyInterpreter.Ast do
  alias MonkeyInterpreter.Ast

  # A program consists of a series of let statements (for now).
  defmodule Program do
    @type t :: %__MODULE__{statements: list(Ast.LetStatement.t())}
    @enforce_keys [:statements]
    defstruct @enforce_keys
  end

  # Each let statement has the name/identifier, and the value. The literal is used for debugging only.
  defmodule LetStatement do
    @type t :: %__MODULE__{literal: binary(), name: Ast.Identifier.t(), value: Ast.Expression.t()}
    @enforce_keys [:literal, :name, :value]
    defstruct @enforce_keys
  end

  defmodule Identifier do
    @type t :: binary()
  end

  # TODO not implemented yet
  defmodule Expression do
    @type t :: %__MODULE__{}
    @enforce_keys []
    defstruct @enforce_keys
  end
end
