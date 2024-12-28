defmodule MonkeyInterpreter.TokenType do
  @type t ::
          :illegal
          | :eof
          | :ident
          | :int
          | :string
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

  @spec is_infix(t()) :: boolean()
  def is_infix(token),
    do: token.type in [:plus, :minus, :asterisk, :slash, :gt, :lt, :eq, :not_eq]

  @spec is_prefix(t()) :: boolean()
  def is_prefix(token), do: token.type in [:bang, :minus]

  # TODO update type spec of returned function
  # Return an operator (a function) that checks the types before doing the operation.
  @spec to_infix_operator(t()) :: fun()
  def to_infix_operator(token) when token.type in [:plus, :minus, :asterisk, :slash, :gt, :lt] do
    operator =
      case token.type do
        :plus -> &Kernel.+/2
        :minus -> &Kernel.-/2
        :asterisk -> &Kernel.*/2
        :slash -> &Kernel.//2
        :gt -> &Kernel.>/2
        :lt -> &Kernel.</2
      end

    fn
      left, right when is_integer(left) and is_integer(right) ->
        {:ok, operator.(left, right)}

      left, right ->
        {:error,
         "type mismatch: #{user_displayed_type(left)} #{token.literal} #{user_displayed_type(right)}"}
    end
  end

  # NOTE: we have to have a separate one for these two operators because they could be any of integer,bool,nil types and because guards are weird in Elixir.
  def to_infix_operator(token) when token.type in [:eq, :not_eq] do
    operator =
      case token.type do
        :eq -> &Kernel.==/2
        :not_eq -> &Kernel.!=/2
      end

    fn
      left, right when is_integer(left) and is_integer(right) ->
        {:ok, operator.(left, right)}

      left, right when is_boolean(left) and is_boolean(right) ->
        {:ok, operator.(left, right)}

      left, right when is_nil(left) and is_nil(right) ->
        {:ok, operator.(left, right)}

      left, right ->
        {:error,
         "type mismatch: #{user_displayed_type(left)} #{token.literal} #{user_displayed_type(right)}"}
    end
  end

  @spec to_prefix_operator(t()) :: fun()
  def to_prefix_operator(token) do
    case token.type do
      :bang ->
        fn
          operand when is_boolean(operand) -> {:ok, !operand}
          # NOTE: explicitly allow "truthy" conversions, just to adhere to the Monkey language.
          operand when is_integer(operand) -> {:ok, !is_truthy(operand)}
          operand -> {:error, "type mismatch: !#{user_displayed_type(operand)}"}
        end

      :minus ->
        fn
          operand when is_integer(operand) -> {:ok, -operand}
          operand -> {:error, "type mismatch: -#{user_displayed_type(operand)}"}
        end
    end
  end

  defp user_displayed_type(value) when is_boolean(value), do: "BOOLEAN"
  defp user_displayed_type(value) when is_integer(value), do: "INTEGER"
  defp user_displayed_type(value) when is_binary(value), do: "STRING"
  defp user_displayed_type(value) when is_nil(value), do: "NULL"

  # TODO double check whether the definition of truthy/falsey in the Monkey language differs from Elixir (the host language).
  @spec is_truthy(any()) :: boolean()
  def is_truthy(value) do
    # TODO actually implement
    if value do
      true
    else
      false
    end
  end
end
