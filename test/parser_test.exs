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
    |> Enum.map(fn {input, expected_identifier, expected_value} ->
      # Check that parsing the program results in a single program statement, with the expected identifier and value.
      program = Lexer.init(input) |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      stmt = program.statements |> List.first()
      assert stmt.name == expected_identifier
      # assert stmt.value == expected_value
    end)
  end
end
