defmodule MonkeyInterpreter.Parser do
  alias MonkeyInterpreter.{Lexer, Token}
  alias MonkeyInterpreter.Ast.{Program, LetStatement, Identifier, Expression}

  @type t :: %__MODULE__{lexer: Lexer.t()}
  @enforce_keys [:lexer]
  defstruct @enforce_keys

  @spec init(Lexer.t()) :: t()
  def init(lexer), do: %__MODULE__{lexer: lexer}

  @spec parse_program(t()) :: Program.t()
  def parse_program(state) do
    # TODO implement
    tokens = state.lexer |> Lexer.all_tokens()

    %Program{statements: parse_tokens(tokens)}
  end

  # Parse all the tokens until we encounter the end of file as the last token.
  defp parse_tokens(tokens, acc \\ [])
  defp parse_tokens([%Token{type: :eof}], acc), do: acc

  # This is a let statement. Parse its tokens into a statement, then continue parsing the rest of the tokens.
  defp parse_tokens([%Token{type: :let} | rest], acc) do
    {stmt, rest} = parse_let_statement(rest)
    new_acc = acc ++ [stmt]
    parse_tokens(rest, new_acc)
  end

  # TODO other kinds of statements/expressions not implemented yet

  defp parse_let_statement(tokens) do
    # We've already seen the "let".
    # Check that there's an identifier next.
    [identifier_token = %Token{type: :ident} | rest] = tokens
    # Then an equal sign.
    [%Token{type: :assign} | rest] = rest
    # Then an expression.
    # TODO for now we ignore the expression result. We just want to make sure we read until the semicolon
    {_expression, rest} = parse_expression(rest)
    # TODO set the expression value and in the literal
    {%LetStatement{
       literal: "let #{identifier_token.literal} = TODO",
       name: identifier_token.literal,
       value: nil
     }, rest}
  end

  # TODO this is currently defined as "look for a semicolon to make sure it's well-formed, then return an empty expression".
  defp parse_expression([%Token{type: :semicolon} | rest]), do: {%Expression{}, rest}
  defp parse_expression([_token | rest]), do: parse_expression(rest)
end
