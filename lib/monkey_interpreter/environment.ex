# Think of this as the available bindings in the current "scope". It may also contain the outer environment.
defmodule MonkeyInterpreter.Environment do
  @type t :: %__MODULE__{bindings: %{String.t() => any()}, outer_environment: t() | nil}
  @enforce_keys [:bindings, :outer_environment]
  defstruct @enforce_keys

  @spec init() :: t()
  def init(), do: %__MODULE__{bindings: %{}, outer_environment: nil}
end
