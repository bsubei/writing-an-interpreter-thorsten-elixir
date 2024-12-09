defmodule MonkeyInterpreter.Lexer do
  @enforce_keys [:text, :current_pos]
  @type t :: %__MODULE__{
          text: binary(),
          current_pos: non_neg_integer()
        }
  defstruct [:text, :current_pos]

  alias MonkeyInterpreter.Token

  # NOTE: here, "ch" must be an integer representing the utf8 codepoint for the character. "ch" is not a binary/string.
  defguard is_letter?(ch) when ch in ?a..?z or ch in ?A..?Z
  defguard is_whitespace?(ch) when ch in [?\s, ?\t, ?\n, ?\r]
  defguard is_digit?(ch) when ch in ?0..?9

  @spec init(binary()) :: t()
  def init(text), do: %__MODULE__{text: text, current_pos: 0}

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
    # TODO this is how the Thorsten book does it in Go:
    # skip all whitespace
    # match on the current char. In some cases, we have to peek ahead and match again (but we can do much better in Elixir)
    # One of these match statements will find something (or an identifier/integer) and return a token.
    # Before we return, call readChar() to move the current position
    state = skip_whitespaces(state)

    {_before, chars_to_lex} = state.text |> String.split_at(state.current_pos)

    token = lex_token(chars_to_lex)

    # Update the cursor position and return new state
    num_bytes_advanced = String.length(token.literal)
    state = update_in(state.current_pos, fn current_pos -> current_pos + num_bytes_advanced end)
    {state, token}
  end

  # Given a string, read one Token from the beginning and return it
  @spec lex_token(binary()) :: Token.t()
  # Single char tokens
  defp lex_token("+" <> _rest), do: Token.init(:plus, "+")
  defp lex_token("-" <> _rest), do: Token.init(:minus, "-")
  defp lex_token("/" <> _rest), do: Token.init(:slash, "/")
  defp lex_token("*" <> _rest), do: Token.init(:asterisk, "*")
  defp lex_token("<" <> _rest), do: Token.init(:lt, "<")
  defp lex_token(">" <> _rest), do: Token.init(:gt, ">")
  defp lex_token(";" <> _rest), do: Token.init(:semicolon, ";")
  defp lex_token("," <> _rest), do: Token.init(:comma, ",")
  defp lex_token("{" <> _rest), do: Token.init(:lbrace, "{")
  defp lex_token("}" <> _rest), do: Token.init(:rbrace, "}")
  defp lex_token("(" <> _rest), do: Token.init(:lparen, "(")
  defp lex_token(")" <> _rest), do: Token.init(:rparen, ")")
  # Multi char tokens, notice the ordering (more specific defined first).
  defp lex_token("==" <> _rest), do: Token.init(:eq, "==")
  defp lex_token("=" <> _rest), do: Token.init(:assign, "=")
  defp lex_token("!=" <> _rest), do: Token.init(:not_eq, "!=")
  defp lex_token("!" <> _rest), do: Token.init(:bang, "!")
  # End of file, nothing left in the string.
  defp lex_token(""), do: Token.init(:eof, "")

  # Fallthrough cases: either a literal, a keyword, an identifier, or an illegal token.

  # If starts with a number, it must be an int literal.
  # TODO currently we only implement integer literals
  # TODO this does not complain about errors like: "x = 123foo". i.e. it lexes 123 separate from foo.
  defp lex_token(<<ch::utf8, _rest::binary>> = text) when is_digit?(ch) do
    Token.init(:int, lex_number(text))
  end

  # If starts with a letter, it must be either a keyword or an identifier
  defp lex_token(<<ch::utf8, _rest::binary>> = text) when is_letter?(ch) or ch == ?_ do
    word = lex_word(text)

    case Map.get(Token.keywords(), word) do
      # It's an identifier/variable name.
      nil -> Token.init(:ident, word)
      # It's a keyword, we know what the token is.
      token -> token
    end
  end

  # We don't know what this is, it must be an illegal token.
  defp lex_token(_text) do
    Token.init(:illegal, nil)
  end

  @spec lex_number(binary(), binary()) :: binary()
  defp lex_number(text, acc \\ "")

  # Recurse if there's more in this number.
  defp lex_number(<<ch::utf8, rest::binary>>, acc) when is_digit?(ch),
    do: lex_number(rest, acc <> <<ch::utf8>>)

  # No more in this number, return.
  defp lex_number(_text, acc), do: acc

  # TODO similar to lex_number, this doesn't handle words with numbers in them.
  @spec lex_word(binary(), binary()) :: binary()
  defp lex_word(text, acc \\ "")

  # Recurse if there's more in this word.
  defp lex_word(<<ch::utf8, rest::binary>>, acc) when is_letter?(ch),
    do: lex_word(rest, acc <> <<ch::utf8>>)

  # No more in this word, return.
  defp lex_word(_text, acc), do: acc

  @spec skip_whitespaces(t()) :: t()
  defp skip_whitespaces(%__MODULE__{text: text, current_pos: current_pos} = state) do
    case String.at(text, current_pos) do
      <<ch::utf8>> when is_whitespace?(ch) ->
        skip_whitespaces(%__MODULE__{state | current_pos: current_pos + 1})

      _ ->
        state
    end
  end
end
