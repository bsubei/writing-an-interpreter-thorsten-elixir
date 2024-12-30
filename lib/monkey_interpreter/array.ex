defmodule MonkeyInterpreter.Array do
  alias MonkeyInterpreter.Object

  # This is the object representation of an array, not the same as ArrayLiteral, which is an AST node.
  @type t :: %__MODULE__{elements: list(Object.t())}
  @enforce_keys [:elements]
  defstruct @enforce_keys
end
