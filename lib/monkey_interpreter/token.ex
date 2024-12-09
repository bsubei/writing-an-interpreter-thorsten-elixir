defmodule MonkeyInterpreter.TokenType do
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

defmodule MonkeyInterpreter.Token do
  @enforce_keys [:type, :literal]
  defstruct @enforce_keys
  @type t :: %__MODULE__{type: TokenType.t(), literal: binary() | nil}

  @spec init(TokenType.t(), binary() | nil) :: t()
  def init(type, literal), do: %__MODULE__{type: type, literal: literal}

  @spec keywords() :: %{binary() => t()}
  def keywords() do
    %{
      "fn" => __MODULE__.init(:function, "fn"),
      "let" => __MODULE__.init(:let, "let"),
      "true" => __MODULE__.init(true, "true"),
      "false" => __MODULE__.init(false, "false"),
      "if" => __MODULE__.init(:if, "if"),
      "else" => __MODULE__.init(:else, "else"),
      "return" => __MODULE__.init(:return, "return")
    }
  end
end
