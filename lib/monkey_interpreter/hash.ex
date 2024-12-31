defmodule MonkeyInterpreter.Hash do
  alias MonkeyInterpreter.Object

  # This is the object representation of a hash, not the same as HashLiteral, which is an AST node.
  @type t :: %__MODULE__{data: %{Object.t() => Object.t()}}
  @enforce_keys [:data]
  defstruct @enforce_keys
end
