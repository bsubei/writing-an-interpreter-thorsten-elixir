defmodule MonkeyInterpreter.Parser do
  alias MonkeyInterpreter.{Ast, Lexer, Token}

  @type t :: %__MODULE__{lexer: Lexer.t()}
  @enforce_keys [:lexer]
  defstruct @enforce_keys

  # This defines the precedence ordering of expressions in the Monkey language.
  # TODO explain why :lowest isn't called :higest instead
  @precedences [:lowest, :equals, :lessgreater, :sum, :product, :prefix, :call]
  @spec precedence_compare(atom(), atom()) :: :lt | :gt | :eq | nil
  def precedence_compare(left, right) do
    case Enum.find_index(@precedences, &(&1 == left)) do
      nil ->
        nil

      left_i ->
        case Enum.find_index(@precedences, &(&1 == right)) do
          nil ->
            nil

          right_i ->
            cond do
              left_i < right_i -> :lt
              left_i == right_i -> :eq
              left_i > right_i -> :gt
            end
        end
    end
  end

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
  # @spec parse_let_statement(list(Token.t())) :: {{:let, Ast.LetStatement.t()}, list(Token.t())}
  defp parse_let_statement(tokens) do
    # We've already seen the "let".
    # Check that there's an identifier next.
    [identifier_token = %Token{type: :ident} | rest] = tokens
    # Then an equal sign.
    [%Token{type: :assign} | rest] = rest
    # Then an expression.
    # TODO hack for now until we can actually parse expressions.
    [assigned_value_token, _semicolon | rest] = rest
    # TODO set the expression value and in the literal
    {
      {:let,
       %Ast.LetStatement{
         literal: "let #{identifier_token.literal} = #{assigned_value_token.literal}",
         identifier: %Ast.Identifier{token: identifier_token, value: identifier_token.literal},
         assigned_value: {:integer, %Ast.IntegerLiteral{token: assigned_value_token, value: 42}}
       }},
      rest
    }
  end

  @spec parse_return_statement(list(Token.t())) :: {Ast.Statement.t(), list(Token.t())}
  defp parse_return_statement(tokens) do
    # We've already seen the "return" keyword.
    # Grab the return value (an expression).
    # TODO hack for now until we can actually parse expressions.
    [expression | rest] = tokens
    [_semicolon | rest] = rest

    # TODO actual return value
    {{:return,
      %Ast.ReturnStatement{
        literal: "return #{expression.literal}",
        return_value: {:integer, %Ast.IntegerLiteral{token: expression, value: 42}}
      }}, rest}
  end

  @spec parse_expression_statement(list(Token.t())) :: {Ast.Statement.t(), list(Token.t())}
  defp parse_expression_statement(tokens) do
    # TODO pass in precedence
    # An expression statement is just an expression and then an optional semicolon.
    {expression, rest} = parse_expression(tokens)

    # TODO wait I'm skipping semicolon twice, once here and once in parse_expression
    rest =
      case rest do
        [%Token{type: :semicolon} | rest] -> rest
        _ -> rest
      end

    {{:expression, %Ast.ExpressionStatement{literal: "TODO", expression: expression}}, rest}
  end

  @spec parse_expression(list(Token.t())) :: {Ast.Expression.t(), list(Token.t())}
  # Base case, stop recursing because we encountered the end of this expression (a semicolon).
  defp parse_expression([%Token{type: :semicolon} | rest]), do: {nil, rest}
  # Parse this token and continue to parse the rest of the expression.
  defp parse_expression([_token | _rest] = tokens) do
    # TODO use precedence and also parse infix
    parse_prefix(tokens)
  end

  @spec parse_prefix(list(Token.t())) :: {Ast.Expression.t(), list(Token.t())}
  defp parse_prefix([%Token{type: :ident} | _rest] = tokens) do
    parse_identifier(tokens)
  end

  defp parse_prefix([%Token{type: :int} | _rest] = tokens) do
    parse_int(tokens)
  end

  defp parse_prefix([%Token{type: :minus} | _rest] = tokens) do
    parse_prefix_expression(tokens)
  end

  defp parse_prefix([%Token{type: :bang} | _rest] = tokens) do
    parse_prefix_expression(tokens)
  end

  # All of the below parse_* functions are just helpers for parse_prefix and have the same spec.
  defp parse_identifier([token | rest]) do
    expression = {:identifier, %Ast.Identifier{token: token, value: token.literal}}
    {expression, rest}
  end

  defp parse_int([token | rest]) do
    {integer, ""} = Integer.parse(token.literal)
    expression = {:integer, %Ast.IntegerLiteral{token: token, value: integer}}
    {expression, rest}
  end

  defp parse_prefix_expression([token | rest]) do
    # Recurse into the right-hand expression.
    {right_expression, rest} = parse_expression(rest)
    expression = {:prefix, %Ast.Prefix{token: token, right_expression: right_expression}}
    {expression, rest}
  end

  # @spec parse_infix(TokenType.t()) :: nil
  # defp parse_infix(:ident) do
  #   nil
  # end
end
