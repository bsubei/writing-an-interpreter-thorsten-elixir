defmodule MonkeyInterpreter.Lexer do
  alias MonkeyInterpreter.Token

  @enforce_keys [:text]
  @type t :: %__MODULE__{text: String.t()}
  defstruct @enforce_keys

  # NOTE: here, "ch" must be an integer representing the utf8 codepoint for the character. "ch" is not a string.
  defguard is_letter?(ch) when ch in ?a..?z or ch in ?A..?Z or ch == ?_
  defguard is_whitespace?(ch) when ch in [?\s, ?\t, ?\n, ?\r]
  defguard is_digit?(ch) when ch in ?0..?9

  @spec init(String.t()) :: t()
  def init(text), do: %__MODULE__{text: text}

  # Given a Lexer, read off all the tokens until you reach an :eof token. Include the eof token in the return.
  @spec all_tokens(t(), list(Token.t())) :: list(Token.t())
  def all_tokens(state, acc \\ [])

  def all_tokens(_state, [%Token{type: :eof} | _rest] = acc) do
    # Reverse the tokens because we prepended.
    Enum.reverse(acc)
  end

  def all_tokens(state, acc) do
    {state, token} = __MODULE__.next_token(state)
    all_tokens(state, [token | acc])
  end

  # TODO we assume ASCII input, not unicode.
  # TODO no error handling, will always return eof after nothing left to read.
  @spec next_token(t()) :: {t(), Token.t()}
  def next_token(state) do
    state = skip_whitespaces(state)

    {token, rest} = lex_token(state.text)
    state = put_in(state.text, rest)

    {state, token}
  end

  # Given a string, read one Token from the beginning and return it
  @spec lex_token(String.t()) :: {Token.t(), String.t()}
  # Single char tokens
  defp lex_token("+" <> rest), do: {Token.init(:plus, "+"), rest}
  defp lex_token("-" <> rest), do: {Token.init(:minus, "-"), rest}
  defp lex_token("/" <> rest), do: {Token.init(:slash, "/"), rest}
  defp lex_token("*" <> rest), do: {Token.init(:asterisk, "*"), rest}
  defp lex_token("<" <> rest), do: {Token.init(:lt, "<"), rest}
  defp lex_token(">" <> rest), do: {Token.init(:gt, ">"), rest}
  defp lex_token(";" <> rest), do: {Token.init(:semicolon, ";"), rest}
  defp lex_token("," <> rest), do: {Token.init(:comma, ","), rest}
  defp lex_token("{" <> rest), do: {Token.init(:lbrace, "{"), rest}
  defp lex_token("}" <> rest), do: {Token.init(:rbrace, "}"), rest}
  defp lex_token("(" <> rest), do: {Token.init(:lparen, "("), rest}
  defp lex_token(")" <> rest), do: {Token.init(:rparen, ")"), rest}
  defp lex_token("[" <> rest), do: {Token.init(:lbracket, "["), rest}
  defp lex_token("]" <> rest), do: {Token.init(:rbracket, "]"), rest}
  # Multi char tokens, notice the ordering (more specific defined first).
  defp lex_token("==" <> rest), do: {Token.init(:eq, "=="), rest}
  defp lex_token("=" <> rest), do: {Token.init(:assign, "="), rest}
  defp lex_token("!=" <> rest), do: {Token.init(:not_eq, "!="), rest}
  defp lex_token("!" <> rest), do: {Token.init(:bang, "!"), rest}
  # End of file, nothing left in the string.
  defp lex_token(""), do: {Token.init(:eof, ""), ""}

  # Fallthrough cases: either a literal, a keyword, an identifier, or an illegal token.

  # If starts with a number, it must be an int literal.
  # TODO currently we only implement integer literals
  # TODO this does not complain about errors like: "x = 123foo". i.e. it lexes 123 separate from foo.
  defp lex_token(<<ch::utf8, _rest::binary>> = text) when is_digit?(ch) do
    {num, rest} = lex_number(text)
    {Token.init(:int, num), rest}
  end

  # If it starts with a double-quote, keep lexing until the closing double-quote (for a string literal).
  defp lex_token(<<"\"", rest::binary>>) do
    {string, rest} = lex_string_literal(rest)
    {Token.init(:string, string), rest}
  end

  # If starts with a letter, it must be either a keyword or an identifier
  defp lex_token(<<ch::utf8, _rest::binary>> = text) when is_letter?(ch) or ch == ?_ do
    {word, rest} = lex_word(text)

    case Map.get(Token.keywords(), word) do
      # It's an identifier/variable name.
      nil -> {Token.init(:ident, word), rest}
      # It's a keyword, we know what the token is.
      token -> {token, rest}
    end
  end

  # We don't know what this is, it must be an illegal token.
  defp lex_token(rest) do
    {Token.init(:illegal, nil), rest}
  end

  @spec lex_string_literal(String.t(), iodata()) :: {String.t(), String.t()}
  defp lex_string_literal(input, acc \\ [])
  defp lex_string_literal(<<"\"", rest::binary>>, acc), do: {IO.iodata_to_binary(acc), rest}
  defp lex_string_literal("", _acc), do: raise("Did not find closing quotes")

  defp lex_string_literal(<<ch::utf8, rest::binary>>, acc),
    do: lex_string_literal(rest, acc ++ [ch])

  @spec lex_number(String.t(), String.t()) :: {String.t(), String.t()}
  defp lex_number(text, acc \\ "")

  # Recurse if there's more in this number.
  defp lex_number(<<ch::utf8, rest::binary>>, acc) when is_digit?(ch),
    do: lex_number(rest, acc <> <<ch::utf8>>)

  # No more in this number, return.
  defp lex_number(rest, acc), do: {acc, rest}

  # TODO similar to lex_number, this doesn't handle words with numbers in them.
  @spec lex_word(String.t(), String.t()) :: {String.t(), String.t()}
  defp lex_word(text, acc \\ "")

  # Recurse if there's more in this word.
  defp lex_word(<<ch::utf8, rest::binary>>, acc) when is_letter?(ch),
    do: lex_word(rest, acc <> <<ch::utf8>>)

  # No more in this word, return.
  defp lex_word(rest, acc), do: {acc, rest}

  @spec skip_whitespaces(t()) :: t()
  defp skip_whitespaces(%__MODULE__{text: <<ch::utf8, rest::binary>>} = state)
       when is_whitespace?(ch),
       do: skip_whitespaces(%__MODULE__{state | text: rest})

  defp skip_whitespaces(state), do: state
end
