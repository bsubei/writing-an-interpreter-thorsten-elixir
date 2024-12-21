defmodule ParserTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser}
  doctest Parser

  test "parser can parse let statements" do
    inputs_and_outputs = [
      {"let x = 5;", "x", 5},
      {"let y = true;", "y", true},
      {"let foobar = y;", "foobar", "y"}
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and value).
    inputs_and_outputs
    |> Enum.each(fn {input, expected_identifier, _expected_value} ->
      # Check that parsing the program results in a single let statement, with the expected identifier and value.
      program = Lexer.init(input) |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:let, stmt} ->
          assert stmt.identifier.literal == expected_identifier
          # assert stmt.value == expected_value
      end
    end)
  end

  test "parser can parse return statements" do
    inputs = [
      "return 5;",
      "return 10;",
      "return 993322;"
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and value).
    inputs
    |> Enum.each(fn input ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()
      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:return, _stmt} ->
          nil
          # assert stmt.return_value == ???
      end
    end)
  end

  test "parser can parse identifier expression statements" do
    program = "foobar;" |> Lexer.init() |> Parser.init() |> Parser.parse_program()
    assert length(program.statements) == 1

    case program.statements |> List.first() do
      {:expression, stmt} ->
        case stmt.expression do
          {:identifier, identifier} ->
            assert identifier.literal == "foobar"
        end
    end
  end
end
