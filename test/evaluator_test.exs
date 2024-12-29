defmodule EvaluatorTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser, Evaluator, Environment}
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
      {"!5 == false", true},
      # Test infix expressions, included grouped ones.
      {"1+1", 1 + 1},
      {"2*4", 2 * 4},
      {"(1 + 1) / 2", (1 + 1) / 2},
      {~s'"foo bar"', "foo bar"}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
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
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
    end)
  end

  test "evaluator correctly handles errors" do
    inputs_and_outputs = [
      {"5 + true;", "type mismatch: INTEGER + BOOLEAN"},
      {"5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"},
      {"-true", "type mismatch: -BOOLEAN"},
      {"true + false", "type mismatch: BOOLEAN + BOOLEAN"},
      {"5; true + false; 5;", "type mismatch: BOOLEAN + BOOLEAN"},
      {"if (10 > 1) { true + false; }", "type mismatch: BOOLEAN + BOOLEAN"},
      {"if (true) { if (true) {return true + false;}; return 0;}",
       "type mismatch: BOOLEAN + BOOLEAN"},
      {"x;", "identifier not found: x"},
      {"let x = y;", "identifier not found: y"},
      {"let x = 5; let y = x; let z = f", "identifier not found: f"},
      {"fn (x, y) {return z;}(1,2)", "identifier not found: z"},
      {~s'"Hello" - "World"', "type mismatch: STRING - STRING"}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:error, reason} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert reason == expected_output
    end)
  end

  test "evaluator can evaluate let statements" do
    inputs_and_outputs = [
      {"let a = 5; a;", 5},
      {"let a = 5 * 5; a;", 25},
      {"let a = 5; let b = a; b;", 5},
      {"let a = 5; let b = a; let c = a + b + 5; c;", 15}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
    end)
  end

  test "evaluator can evaluate function application" do
    inputs_and_outputs = [
      {"let identity = fn(x) { x; }; identity(5);", 5},
      {"let identity = fn(x) { return x; }; identity(5);", 5},
      {"let double = fn(x) { x * 2; }; double(5);", 10},
      {"let add = fn(x, y) { x + y; }; add(5, 5);", 10},
      {"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20},
      {"fn(x) { x; }(5)", 5}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
    end)
  end

  test "evaluator can evaluate with scoping rules respected" do
    inputs_and_outputs = [
      {"
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;

  first + second + third;
};

ourFunction(20) + first + second;
      ", 70}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
    end)
  end

  test "evaluator can evaluate string concatenation" do
    inputs_and_outputs = [
      {~s'"foo" + "bar"', "foobar"},
      {~s'let foo = "Hello"; let bar = "World"; foo + ", " + bar + "!"', "Hello, World!"}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      {:ok, object, _environment} =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()
        |> Evaluator.evaluate(Environment.init())

      assert object == expected_output
    end)
  end
end
