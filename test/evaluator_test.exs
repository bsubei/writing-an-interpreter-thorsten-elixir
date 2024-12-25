defmodule EvaluatorTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser, Evaluator}
  doctest Evaluator

  test "evaluator can evaluate basic expressions" do
    inputs_and_outputs = [
      {"!true", false},
      {"!!true", true},
      {"42", 42},
      {"-42", -42},
      {"--42", 42}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      result =
        input |> Lexer.init() |> Parser.init() |> Parser.parse_program() |> Evaluator.eval()

      assert result == expected_output
    end)
  end
end
