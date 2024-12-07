defmodule Lexer do
  defstruct []

  def init(_), do: nil

  def next_token(state), do: {state, Token.init(:let, "let")}
end
