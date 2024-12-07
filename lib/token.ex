defmodule Token do
  @enforce_keys [:type, :literal]
  @type t :: %__MODULE__{type: atom(), literal: binary()}

  defstruct [:type, :literal]

  @spec init(atom(), binary()) :: t()
  def init(type, literal), do: %Token{type: type, literal: literal}
end
