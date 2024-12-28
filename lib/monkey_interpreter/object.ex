defmodule MonkeyInterpreter.Object do
  @type t :: boolean() | integer() | MonkeyInterpreter.Function.t() | nil
end
