defmodule MonkeyInterpreter.Ast do
  alias MonkeyInterpreter.Ast

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

  # Each let statement has the identifier and the value. The literal is used for debugging only.
  defmodule LetStatement do
    @type t :: %__MODULE__{
            literal: String.t(),
            identifier: Ast.Identifier.t(),
            value: Ast.Expression.t()
          }
    @enforce_keys [:literal, :identifier, :value]
    defstruct @enforce_keys
  end

  # Each return statement has the expression value being returned. The literal is used for debugging only.
  defmodule ReturnStatement do
    @type t :: %__MODULE__{literal: String.t(), return_value: Ast.Expression.t()}
    @enforce_keys [:literal, :return_value]
    defstruct @enforce_keys
  end

  # Each expression statement only consists of an expression.
  defmodule ExpressionStatement do
    @type t :: %__MODULE__{literal: String.t(), expression: Ast.Expression.t()}
    @enforce_keys [:literal, :expression]
    defstruct @enforce_keys
  end

  # TODO more
  defmodule Expression do
    @type t ::
            {:identifer, Ast.Identifier.t()}
            | {:boolean, Ast.Boolean.t()}
            | {:integer, Ast.IntegerLiteral.t()}
  end

  # An identifier is a kind of expression and consists of only a token, always with an :ident type.
  # The token also has a "literal" field, which contains the identifier name.
  defmodule Identifier do
    @type t :: Ast.Token.t()
  end

  defmodule Boolean do
    @type t :: Ast.Token.t()
  end

  defmodule IntegerLiteral do
    @type t :: Ast.Token.t()
  end
end
