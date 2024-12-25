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

  @spec is_infix(t()) :: boolean()
  def is_infix(token_type),
    do: token_type in [:plus, :minus, :asterisk, :slash, :gt, :lt, :eq, :not_eq]

  @spec to_infix_operator(t()) :: fun()
  def to_infix_operator(token_type) do
    case token_type do
      :plus -> &Kernel.+/2
      :minus -> &Kernel.-/2
      :asterisk -> &Kernel.*/2
      :slash -> &Kernel.//2
      :gt -> &Kernel.>/2
      :lt -> &Kernel.</2
      :eq -> &Kernel.==/2
      :not_eq -> &Kernel.!=/2
    end
  end

  @spec is_prefix(t()) :: boolean()
  def is_prefix(token_type), do: token_type in [:bang, :minus]

  @spec to_prefix_operator(t()) :: fun()
  def to_prefix_operator(token_type) do
    case token_type do
      :bang -> &Kernel.!/1
      :minus -> &Kernel.-/1
    end
  end
end

defmodule MonkeyInterpreter.TokenPrecedence do
  @type t :: :lowest | :equals | :lessgreater | :sum | :product | :prefix | :call
  # This defines the precedence ordering of expressions in the Monkey language.
  @precedences [:lowest, :equals, :lessgreater, :sum, :product, :prefix, :call]

  @spec from_token_type(MonkeyInterpreter.TokenType.t()) :: t() | nil
  def from_token_type(token_type) do
    case token_type do
      :eq -> :equals
      :not_eq -> :equals
      :lt -> :lessgreater
      :gt -> :lessgreater
      :plus -> :sum
      :minus -> :sum
      :slash -> :product
      :asterisk -> :product
      :lparen -> :call
      _ -> nil
    end
  end

  @spec compare(t(), t()) :: :lt | :gt | :eq
  def compare(left, right) do
    case Enum.find_index(@precedences, &(&1 == left)) do
      nil ->
        raise("Cannot find precedence order for : #{left}")

      left_i ->
        case Enum.find_index(@precedences, &(&1 == right)) do
          nil ->
            raise("Cannot find precedence order for : #{right}")

          right_i ->
            cond do
              left_i < right_i -> :lt
              left_i == right_i -> :eq
              left_i > right_i -> :gt
            end
        end
    end
  end
end

defmodule MonkeyInterpreter.Token do
  alias MonkeyInterpreter.TokenType
  @type t :: %__MODULE__{type: TokenType.t(), literal: String.t() | nil}
  @enforce_keys [:type, :literal]
  defstruct @enforce_keys

  @spec init(TokenType.t(), String.t() | nil) :: t()
  def init(type, literal), do: %__MODULE__{type: type, literal: literal}

  @spec keywords() :: %{String.t() => t()}
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
