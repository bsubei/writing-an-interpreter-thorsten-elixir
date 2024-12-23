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
            | {:expression_statement, Ast.ExpressionStatement.t()}
            | {:block_statement, Ast.BlockStatement.t()}
  end

  # TODO remove literal from everywhere except the tokens themselves.
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

  defmodule BlockStatement do
    @type t :: %__MODULE__{literal: String.t() | nil, statements: list(Ast.Statement.t())}
    @enforce_keys [:literal, :statements]
    defstruct @enforce_keys
  end

  defmodule Expression do
    @type t ::
            {:identifier, Ast.Identifier.t()}
            | {:boolean, Ast.Boolean.t()}
            | {:integer, Ast.IntegerLiteral.t()}
            | {:grouped, Ast.GroupedExpression.t()}
            | {:if, Ast.IfExpression.t()}
            | {:function_literal, Ast.FunctionLiteral.t()}
            | {:prefix, Ast.Prefix.t()}
            | {:infix, Ast.Infix.t()}
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

  defmodule GroupedExpression do
    @type t :: %__MODULE__{expression: Ast.Expression.t()}
    @enforce_keys [:expression]
    defstruct @enforce_keys
  end

  # Note that "if" is an expression, not a statement.
  defmodule IfExpression do
    @type t :: %__MODULE__{
            condition: Ast.Expression.t(),
            consequence: Ast.BlockStatement.t(),
            alternative: Ast.BlockStatement.t() | nil
          }
    @enforce_keys [:condition, :consequence, :alternative]
    defstruct @enforce_keys
  end

  defmodule FunctionLiteral do
    @type t :: %__MODULE__{
            parameters: list(Ast.Identifier.t()),
            body: Ast.BlockStatement.t()
          }
    @enforce_keys [:parameters, :body]
    defstruct @enforce_keys
  end

  # A Prefix expression contains the operator token and the "right" expression, which could be any of the expression types.
  defmodule Prefix do
    @type t :: %__MODULE__{operator_token: Token.t(), right_expression: Ast.Expression.t()}
    @enforce_keys [:operator_token, :right_expression]
    defstruct @enforce_keys
  end

  defmodule Infix do
    @type t :: %__MODULE__{
            operator_token: Token.t(),
            left_expression: Ast.Expression.t(),
            right_expression: Ast.Expression.t()
          }
    @enforce_keys [:operator_token, :left_expression, :right_expression]
    defstruct @enforce_keys
  end
end
