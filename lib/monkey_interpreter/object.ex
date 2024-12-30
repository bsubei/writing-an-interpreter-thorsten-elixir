defmodule MonkeyInterpreter.Object do
  @type t ::
          boolean()
          | integer()
          | MonkeyInterpreter.Function.t()
          | MonkeyInterpreter.Builtin.t()
          | MonkeyInterpreter.Array.t()
          | nil
end
