defmodule MonkeyInterpreter.Parser do
  alias MonkeyInterpreter.{Ast, Lexer, Token, TokenPrecedence}

  @type t :: %__MODULE__{lexer: Lexer.t()}
  @enforce_keys [:lexer]
  defstruct @enforce_keys

  @spec init(Lexer.t()) :: t()
  def init(lexer), do: %__MODULE__{lexer: lexer}

  @spec parse_program(t()) :: Ast.Program.t()
  def parse_program(state) do
    tokens = state.lexer |> Lexer.all_tokens()
    %Ast.Program{statements: parse_tokens(tokens)}
  end

  # Parse all the tokens until we encounter the end of file as the last token.
  @spec parse_tokens(list(Token.t()), list(Ast.Statement.t())) ::
          list(Ast.Statement.t())
  defp parse_tokens(tokens, acc \\ [])
  defp parse_tokens([%Token{type: :eof}], acc), do: acc

  # TODO if all these parse_tokens arms look the same, it may be better to get rid of the extra layer of indirection.

  # This is a let statement. Parse its tokens into a statement, then continue parsing the rest of the tokens.
  defp parse_tokens([%Token{type: :let} | rest], acc) do
    {stmt, rest} = parse_let_statement(rest)
    new_acc = acc ++ [stmt]
    parse_tokens(rest, new_acc)
  end

  # This is a return statement. Parse its tokens into a statement, then continue parsing the rest of the tokens.
  defp parse_tokens([%Token{type: :return} | rest], acc) do
    {stmt, rest} = parse_return_statement(rest)
    new_acc = acc ++ [stmt]
    parse_tokens(rest, new_acc)
  end

  # In the general case, assume this is an expression statement.
  defp parse_tokens(tokens, acc) do
    {stmt, rest} = parse_expression_statement(tokens)
    new_acc = acc ++ [stmt]
    parse_tokens(rest, new_acc)
  end

  @spec parse_let_statement(list(Token.t())) :: {Ast.Statement.t(), list(Token.t())}
  defp parse_let_statement(tokens) do
    # We've already seen the "let".
    # Check that there's an identifier next.
    [identifier_token = %Token{type: :ident} | rest] = tokens
    # Then an equal sign.
    [%Token{type: :assign} | rest] = rest
    # Then an expression.
    {assigned_value_expression, rest} = parse_expression(rest, :lowest)

    # Skip over the semicolon if you see it.
    rest =
      case rest do
        [%Token{type: :semicolon} | rest] -> rest
        _ -> rest
      end

    {
      {:let,
       %Ast.LetStatement{
         # I should probably get rid of the literal field in expressions/statements entirely
         literal: "unused",
         identifier: %Ast.Identifier{token: identifier_token, value: identifier_token.literal},
         assigned_value: assigned_value_expression
       }},
      rest
    }
  end

  @spec parse_return_statement(list(Token.t())) :: {Ast.Statement.t(), list(Token.t())}
  defp parse_return_statement(tokens) do
    # We've already seen the "return" keyword.
    # Grab the return value (an expression).
    {return_value, rest} = parse_expression(tokens, :lowest)

    # Skip over the semicolon if you see it.
    rest =
      case rest do
        [%Token{type: :semicolon} | rest] -> rest
        _ -> rest
      end

    {{:return, %Ast.ReturnStatement{literal: "unused", return_value: return_value}}, rest}
  end

  @spec parse_expression_statement(list(Token.t())) :: {Ast.Statement.t(), list(Token.t())}
  defp parse_expression_statement(tokens) do
    # An expression statement is just an expression and then an optional semicolon.
    {expression, rest} = parse_expression(tokens, :lowest)

    # Skip over the semicolon if you see it.
    rest =
      case rest do
        [%Token{type: :semicolon} | rest] -> rest
        _ -> rest
      end

    {{:expression_statement, %Ast.ExpressionStatement{literal: "unused", expression: expression}},
     rest}
  end

  @spec parse_expression(list(Token.t()), TokenPrecedence.t()) ::
          {Ast.Expression.t(), list(Token.t())}
  # Base case, stop recursing because we encountered the end of this expression (a semicolon).
  # defp parse_expression([%Token{type: :semicolon} | rest]), do: {nil, rest}
  # Parse this token and continue to parse the rest of the expression.
  defp parse_expression([_token | _rest] = tokens, precedence) do
    # Always parse one prefix expression.
    {expression, rest} = parse_prefix(tokens)

    # Then, parse however many infix expressions still exist.
    parse_infix_recurse(expression, rest, precedence)

    # How the book does it:
    # If a prefix fn exists, call it to get the left expression (this can recurse).
    # Then, loop by calling the infix fn on the next token until either:
    #  1. the next token has no infix fn (or is a semicolon), or
    #  2. the given precedence >= the next token's precedence
    # At this point, we return whatever resulting expression we got (from the infix loop, or the one prefix call).
  end

  defp parse_infix_recurse(expression, rest, precedence) do
    {new_expr, rest} = parse_infix_expression(expression, rest, precedence)

    case new_expr do
      # Stop recursing and return the expression we were given. This doesn't consume any tokens.
      nil -> {expression, rest}
      # Keep recursing because there's more infix operations.
      # TODO I think this precedence is wrong. We probably have to get the new precedence from the returned expression
      _ -> parse_infix_recurse(new_expr, rest, precedence)
    end
  end

  @spec parse_prefix(list(Token.t())) :: {Ast.Expression.t(), list(Token.t())}
  defp parse_prefix([%Token{type: :ident} = token | rest]) do
    expression = {:identifier, %Ast.Identifier{token: token, value: token.literal}}
    {expression, rest}
  end

  defp parse_prefix([%Token{type: :int} = token | rest]) do
    {integer, ""} = Integer.parse(token.literal)
    expression = {:integer, %Ast.IntegerLiteral{token: token, value: integer}}
    {expression, rest}
  end

  defp parse_prefix([%Token{type: true} = token | rest]) do
    {{:boolean, %Ast.Boolean{token: token, value: true}}, rest}
  end

  defp parse_prefix([%Token{type: false} = token | rest]) do
    {{:boolean, %Ast.Boolean{token: token, value: false}}, rest}
  end

  defp parse_prefix([%Token{type: :minus} | _rest] = tokens) do
    parse_prefix_expression(tokens)
  end

  defp parse_prefix([%Token{type: :bang} | _rest] = tokens) do
    parse_prefix_expression(tokens)
  end

  defp parse_prefix_expression([token | rest]) do
    # Recurse into the right-hand expression.
    {right_expression, rest} = parse_expression(rest, :prefix)
    expression = {:prefix, %Ast.Prefix{operator_token: token, right_expression: right_expression}}
    {expression, rest}
  end

  @spec parse_infix_expression(Ast.Expression.t(), list(Token.t()), TokenPrecedence.t()) ::
          {Ast.Expression.t() | nil, list(Token.t())}
  defp parse_infix_expression(left_expression, [operator_token | rest] = tokens, precedence) do
    operator_precedence = TokenPrecedence.from_token_type(operator_token.type)

    is_precedence_satisfied =
      case operator_precedence do
        nil -> false
        _ -> TokenPrecedence.compare(precedence, operator_precedence) == :lt
      end

    operator_is_infix =
      operator_token.type in [:plus, :minus, :asterisk, :slash, :gt, :lt, :eq, :not_eq]

    if is_precedence_satisfied and operator_is_infix do
      precedence = TokenPrecedence.from_token_type(operator_token.type)

      # Recurse into the right-hand expression using the precedence of the infix operator token.
      {right_expression, rest} = parse_expression(rest, precedence)

      expression =
        {:infix,
         %Ast.Infix{
           operator_token: operator_token,
           left_expression: left_expression,
           right_expression: right_expression
         }}

      {expression, rest}
    else
      # This token either doesn't satisfy the precedence condition OR is not actually an infix operator. Return nil and don't consume any tokens.
      {nil, tokens}
    end
  end
end
