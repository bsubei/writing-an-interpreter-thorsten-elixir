defmodule ParserTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser, Token, Ast}
  doctest Parser

  test "parser can parse let statements" do
    inputs_and_outputs = [
      {"let x = 5;", "x",
       {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "5"), value: 5}}},
      {"let y = true;", "y",
       {:boolean, %Ast.Boolean{token: Token.init(true, "true"), value: true}}},
      {"let foobar = y;", "foobar",
       {:identifier, %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}}}
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and value).
    inputs_and_outputs
    |> Enum.each(fn {input, expected_identifier, _expected_assigned_value} ->
      # Check that parsing the program results in a single let statement, with the expected identifier and value.
      program = Lexer.init(input) |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:let, let_stmt} ->
          assert let_stmt.identifier.token.literal == expected_identifier
          assert let_stmt.identifier.value == expected_identifier

          # TODO implement
          # assert let_stmt.assigned_value == expected_assigned_value
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
        {:return, stmt} ->
          # We expect the literal statement to be everything except the semicolon.
          expected_literal = input |> String.slice(0..(String.length(input) - 2))
          assert stmt.literal == expected_literal
          # TODO more checking
          # {:integer, _expression} = stmt.return_value
      end
    end)
  end

  test "parser can parse identifier expression statements" do
    program = "foobar;" |> Lexer.init() |> Parser.init() |> Parser.parse_program()
    assert length(program.statements) == 1

    case program.statements |> List.first() do
      {:expression_statement, stmt} ->
        case stmt.expression do
          {:identifier, expression} ->
            assert expression.token == %Token{type: :ident, literal: "foobar"}
            assert expression.value == "foobar"
        end
    end
  end

  test "parser can parse literal expression statements" do
    program = "5;" |> Lexer.init() |> Parser.init() |> Parser.parse_program()
    assert length(program.statements) == 1

    case program.statements |> List.first() do
      {:expression_statement, stmt} ->
        case stmt.expression do
          {:integer, integer_literal} ->
            assert integer_literal.token == %Token{type: :int, literal: "5"}
            assert integer_literal.value == 5
        end
    end
  end

  test "parser can parse nested prefix expressions" do
    inputs_and_outputs = [
      {"!5;", Token.init(:bang, "!"), 5, 1},
      {"!!5;", Token.init(:bang, "!"), 5, 2},
      {"--15;", Token.init(:minus, "-"), 15, 2},
      {"!!!!42;", Token.init(:bang, "!"), 42, 4}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_operator, expected_value, num_nesting} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()
      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          check_nested_expected_expression(
            stmt.expression,
            expected_operator,
            expected_value,
            num_nesting
          )
      end
    end)
  end

  defp check_nested_expected_expression(expression, _expected_operator, expected_value, 0) do
    case expression do
      {:integer, integer_literal} -> assert integer_literal.value == expected_value
    end
  end

  defp check_nested_expected_expression(
         expression,
         expected_operator,
         expected_value,
         num_nesting_remaining
       ) do
    case expression do
      {:prefix, prefix_expression} ->
        assert prefix_expression.operator_token == expected_operator

        check_nested_expected_expression(
          prefix_expression.right_expression,
          expected_operator,
          expected_value,
          num_nesting_remaining - 1
        )
    end
  end

  test "parser can parse infix expressions" do
    inputs_and_outputs = [
      {"42 + 5;", 42, Token.init(:plus, "+"), 5},
      {"5 - 5;", 5, Token.init(:minus, "-"), 5},
      {"5 * 5;", 5, Token.init(:asterisk, "*"), 5},
      {"5 / 5;", 5, Token.init(:slash, "/"), 5},
      {"5 > 5;", 5, Token.init(:gt, ">"), 5},
      {"5 < 5;", 5, Token.init(:lt, "<"), 5},
      {"5 == 5;", 5, Token.init(:eq, "=="), 5},
      {"5 != 5;", 5, Token.init(:not_eq, "!="), 5}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_left_value, expected_operator, expected_right_value} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          case stmt.expression do
            {:infix, infix_expression} ->
              assert infix_expression.operator_token == expected_operator

              case infix_expression.left_expression do
                {:integer, integer_literal} -> assert integer_literal.value == expected_left_value
              end

              case infix_expression.right_expression do
                {:integer, integer_literal} ->
                  assert integer_literal.value == expected_right_value
              end
          end
      end
    end)
  end
end
