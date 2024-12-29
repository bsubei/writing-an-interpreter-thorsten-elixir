defmodule MonkeyInterpreter.Object do
  @type t ::
          boolean()
          | integer()
          | MonkeyInterpreter.Function.t()
          | MonkeyInterpreter.Builtin.t()
          | nil
end
