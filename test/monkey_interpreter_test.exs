defmodule MonkeyInterpreterTest do
  use ExUnit.Case
  doctest MonkeyInterpreter

  test "greets the world" do
    assert MonkeyInterpreter.hello() == :world
  end
end
