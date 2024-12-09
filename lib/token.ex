defmodule TokenType do
  @type t ::
          :illegal
          | :eof
          | :ident
          | :int
          | :assign
          | :plus
          | :minus
          | :bang
          | :asterisk
          | :slash
          | :lt
          | :gt
          | :eq
          | :not_eq
          | :comma
          | :semicolon
          | :lparen
          | :rparen
          | :lbrace
          | :rbrace
          | :function
          | :let
          | true
          | false
          | :if
          | :else
          | :return
end

defmodule Token do
  @enforce_keys [:type, :literal]
  defstruct @enforce_keys
  @type t :: %__MODULE__{type: TokenType.t(), literal: binary() | nil}

  @spec init(TokenType.t(), binary() | nil) :: t()
  def init(type, literal), do: %Token{type: type, literal: literal}

  @spec keywords() :: %{binary() => t()}
  def keywords() do
    %{
      "fn" => Token.init(:function, "fn"),
      "let" => Token.init(:let, "let"),
      "true" => Token.init(true, "true"),
      "false" => Token.init(false, "false"),
      "if" => Token.init(:if, "if"),
      "else" => Token.init(:else, "else"),
      "return" => Token.init(:return, "return")
    }
  end
end
