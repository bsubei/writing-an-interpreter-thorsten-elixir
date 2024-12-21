defmodule MonkeyInterpreter.Parser do
  alias MonkeyInterpreter.{Lexer, Token}

  alias MonkeyInterpreter.Ast.{
    Program,
    LetStatement,
    Expression,
    ReturnStatement,
    ExpressionStatement,
    Statement
  }

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

  @spec parse_program(t()) :: Program.t()
  def parse_program(state) do
    tokens = state.lexer |> Lexer.all_tokens()
    %Program{statements: parse_tokens(tokens)}
  end

  # Parse all the tokens until we encounter the end of file as the last token.
  @spec parse_tokens(list(Token.t()), list(Statement.t())) ::
          list(Statement.t())
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

  @spec parse_let_statement(list(Token.t())) :: {Statement.t(), list(Token.t())}
  defp parse_let_statement(tokens) do
    # We've already seen the "let".
    # Check that there's an identifier next.
    [identifier_token = %Token{type: :ident} | rest] = tokens
    # Then an equal sign.
    [%Token{type: :assign} | rest] = rest
    # Then an expression.
    # TODO hack for now until we can actually parse expressions.
    [_expression, _semicolon | rest] = rest
    # TODO set the expression value and in the literal
    {
      {:let,
       %LetStatement{
         literal: "let #{identifier_token.literal} = TODO",
         identifier: identifier_token,
         value: %{}
       }},
      rest
    }
  end

  @spec parse_return_statement(list(Token.t())) :: {Statement.t(), list(Token.t())}
  defp parse_return_statement(tokens) do
    # We've already seen the "return" keyword.
    # Grab the return value (an expression).
    # TODO hack for now until we can actually parse expressions.
    [_expression, _semicolon | rest] = tokens
    {{:return, %ReturnStatement{literal: "return TODO", return_value: %{}}}, rest}
  end

  @spec parse_expression_statement(list(Token.t())) :: {Statement.t(), list(Token.t())}
  defp parse_expression_statement(tokens) do
    # TODO pass in precedence
    # An expression statement is just an expression and then an optional semicolon.
    {expression, rest} = parse_expression(tokens)

    rest =
      case rest do
        [%Token{type: :semicolon} | rest] -> rest
        _ -> rest
      end

    {{:expression, %ExpressionStatement{literal: "TODO", expression: expression}}, rest}
  end

  @spec parse_expression(list(Token.t())) :: {Expression.t(), list(Token.t())}
  # Base case, stop recursing because we encountered the end of this expression (a semicolon).
  defp parse_expression([%Token{type: :semicolon} | rest]), do: {nil, rest}
  # Parse this token and continue to parse the rest of the expression.
  defp parse_expression([token | rest]) do
    # TODO use precedence and actually recurse
    {parse_prefix(token), rest}
  end

  @spec parse_prefix(Token.t()) :: Expression.t()
  defp parse_prefix(%Token{type: :ident} = token) do
    parse_identifier(token)
  end

  defp parse_prefix(%Token{type: :int} = token) do
    parse_int(token)
  end

  defp parse_prefix(_token), do: raise("Unimplemented")

  # @spec parse_infix(TokenType.t()) :: nil
  # defp parse_infix(:ident) do
  #   nil
  # end

  @spec parse_identifier(Token.t()) :: Expression.t()
  def parse_identifier(token), do: {:identifier, token}

  @spec parse_int(Token.t()) :: Expression.t()
  def parse_int(token), do: {:integer, token}
end
