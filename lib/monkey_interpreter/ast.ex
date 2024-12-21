defmodule MonkeyInterpreter.Ast do
  alias MonkeyInterpreter.Ast
  alias MonkeyInterpreter.Token

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
            | {:expression, Ast.ExpressionStatement.t()}
  end

  # Each let statement has the identifier and the value to be assigned. The literal is used for debugging only.
  defmodule LetStatement do
    @type t :: %__MODULE__{
            literal: String.t() | nil,
            identifier: Ast.Identifier.t(),
            assigned_value: Ast.Expression.t()
          }
    @enforce_keys [:literal, :identifier, :assigned_value]
    defstruct @enforce_keys
  end

  # Each return statement has the expression value being returned. The literal is used for debugging only.
  defmodule ReturnStatement do
    @type t :: %__MODULE__{literal: String.t() | nil, return_value: Ast.Expression.t()}
    @enforce_keys [:literal, :return_value]
    defstruct @enforce_keys
  end

  # Each expression statement only consists of an expression.
  defmodule ExpressionStatement do
    @type t :: %__MODULE__{literal: String.t() | nil, expression: Ast.Expression.t()}
    @enforce_keys [:literal, :expression]
    defstruct @enforce_keys
  end

  # TODO more
  defmodule Expression do
    @type t ::
            {:identifier, Ast.Identifier.t()}
            | {:boolean, Ast.Boolean.t()}
            | {:integer, Ast.IntegerLiteral.t()}
  end

  # An identifier is a kind of expression and consists of only a token, always with an :ident type.
  # The token also has a "literal" field, which contains the identifier name.
  defmodule Identifier do
    @type t :: %__MODULE__{token: Token.t(), value: String.t()}
    @enforce_keys [:token, :value]
    defstruct @enforce_keys
  end

  defmodule Boolean do
    @type t :: %__MODULE__{token: Token.t(), value: boolean()}
    @enforce_keys [:token, :value]
    defstruct @enforce_keys
  end

  defmodule IntegerLiteral do
    @type t :: %__MODULE__{token: Token.t(), value: integer()}
    @enforce_keys [:token, :value]
    defstruct @enforce_keys
  end
end
