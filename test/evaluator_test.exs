defmodule EvaluatorTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser, Evaluator}
  doctest Evaluator

  test "evaluator can evaluate prefix and infix expressions" do
    inputs_and_outputs = [
      {"!true", false},
      {"!!true", true},
      {"42", 42},
      {"-42", -42},
      {"--42", 42},
      # Test truthy/falsey conversions.
      {"!5", false},
      {"!!5", true},
      # Test infix expressions, included grouped ones.
      {"1+1", 1 + 1},
      {"2*4", 2 * 4},
      {"(1 + 1) / 2", (1 + 1) / 2}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      result =
        input |> Lexer.init() |> Parser.init() |> Parser.parse_program() |> Evaluator.evaluate()

      assert result == expected_output
    end)
  end

  test "evaluator can evaluate if expressions" do
    inputs_and_outputs = [
      {"if (true) { 42 }", 42},
      {"if (false) { 43 }", nil},
      {"if (1) { 10 }", 10},
      {"if (1 < 2) { 5 }", 5},
      {"if (1 < 2) { 5 } else { 66 }", 5},
      {"if (1 > 2) { 5 } else { 66 }", 66},
      # Test that the first-encountered return will return "early".
      {"if (true) { if (true) {return 10;}; return 0;}", 10}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      result =
        input |> Lexer.init() |> Parser.init() |> Parser.parse_program() |> Evaluator.evaluate()

      assert result == expected_output
    end)
  end
end
