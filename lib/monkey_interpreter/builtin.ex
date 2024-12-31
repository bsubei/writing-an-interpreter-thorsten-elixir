defmodule MonkeyInterpreter.Builtin do
  alias MonkeyInterpreter.{Builtin, Object, Token, Array}

  @type t :: %__MODULE__{func: Builtin.Function.t()}
  @enforce_keys [:func]
  defstruct @enforce_keys

  @spec builtins() :: %{binary() => Builtin.t()}
  def builtins() do
    %{
      "len" =>
        make_builtin(fn
          [value] when is_binary(value) ->
            {:ok, String.length(value)}

          [%Array{elements: elements}] ->
            {:ok, length(elements)}

          [value] ->
            {:error, "argument to `len` not supported, got #{Token.user_displayed_type(value)}"}

          args ->
            {:error, "wrong number of arguments. got=#{length(args)}, want=1"}
        end)
    }
  end

  defp make_builtin(func) do
    %__MODULE__{func: func}
  end

  # NOTE: because variadic functions are not supported in Elixir/Erlang, we use a list of arguments instead and pattern match on them (it ends up being cleaner anyways).
  defmodule Function do
    @type t :: (list(Object.t()) -> {:ok, Object.t()} | {:error, String.t()})
  end
end
