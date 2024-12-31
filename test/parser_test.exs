defmodule ParserTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Parser, Token, Ast}
  doctest Parser

  test "parser can parse let statements" do
    inputs_and_outputs = [
      {"let x = 5;", %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"},
       {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "5"), value: 5}}},
      {"let y = true;", %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"},
       {:boolean, %Ast.Boolean{token: Token.init(true, "true"), value: true}}},
      {"let foobar = y;", %Ast.Identifier{token: Token.init(:ident, "foobar"), value: "foobar"},
       {:identifier, %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}}}
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and assigned_value expression).
    inputs_and_outputs
    |> Enum.each(fn {input, expected_identifier, expected_assigned_value} ->
      # Check that parsing the program results in a single let statement, with the expected identifier and value.
      program = Lexer.init(input) |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:let_statement, let_stmt} ->
          assert let_stmt.identifier == expected_identifier
          assert let_stmt.assigned_value == expected_assigned_value
      end
    end)
  end

  test "parser can parse return statements" do
    inputs_and_outputs = [
      {"return 5;", {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "5"), value: 5}}},
      {"return y;", {:identifier, %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}}},
      {"return -993322;",
       {:prefix,
        %Ast.Prefix{
          operator_token: Token.init(:minus, "-"),
          right_expression:
            {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "993322"), value: 993_322}}
        }}}
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and value).
    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()
      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:return_statement, stmt} -> assert stmt.return_value == expected_output
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

  test "parser can parse int literal expression statements" do
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

  test "parser can parse string literal expression statements" do
    program = ~s'"foobar"' |> Lexer.init() |> Parser.init() |> Parser.parse_program()
    assert length(program.statements) == 1

    case program.statements |> List.first() do
      {:expression_statement, stmt} ->
        case stmt.expression do
          {:string, string_literal} ->
            assert string_literal.token == %Token{type: :string, literal: "foobar"}
            assert string_literal.value == "foobar"
        end
    end
  end

  test "parser can parse array literal expression statements" do
    program = "[1, 2 * 2, 3 + 3]" |> Lexer.init() |> Parser.init() |> Parser.parse_program()
    assert length(program.statements) == 1

    case program.statements |> List.first() do
      {:expression_statement, stmt} ->
        case stmt.expression do
          {:array, array_literal} ->
            assert array_literal.token == %Token{type: :lbracket, literal: "["}

            assert array_literal.elements == [
                     {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}},
                     {:infix,
                      %Ast.Infix{
                        operator_token: Token.init(:asterisk, "*"),
                        left_expression:
                          {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "2"), value: 2}},
                        right_expression:
                          {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "2"), value: 2}}
                      }},
                     {:infix,
                      %Ast.Infix{
                        operator_token: Token.init(:plus, "+"),
                        left_expression:
                          {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "3"), value: 3}},
                        right_expression:
                          {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "3"), value: 3}}
                      }}
                   ]
        end
    end
  end

  test "parser can parse hash literal expression statements" do
    inputs_and_outputs = [
      {~s'{"one": 1, "two": 2, "three": 3}',
       %{
         {:string, %Ast.StringLiteral{token: Token.init(:string, "one"), value: "one"}} =>
           {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}},
         {:string, %Ast.StringLiteral{token: Token.init(:string, "two"), value: "two"}} =>
           {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "2"), value: 2}},
         {:string, %Ast.StringLiteral{token: Token.init(:string, "three"), value: "three"}} =>
           {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "3"), value: 3}}
       }},
      {"{}", %{}},
      {~s'{"one": 0 + 1, "two": 10 - 8, "three" : 15 / 5}',
       %{
         {:string, %Ast.StringLiteral{token: Token.init(:string, "one"), value: "one"}} =>
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:plus, "+"),
              left_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "0"), value: 0}},
              right_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}}
            }},
         {:string, %Ast.StringLiteral{token: Token.init(:string, "two"), value: "two"}} =>
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:minus, "-"),
              left_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "10"), value: 10}},
              right_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "8"), value: 8}}
            }},
         {:string, %Ast.StringLiteral{token: Token.init(:string, "three"), value: "three"}} =>
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:slash, "/"),
              left_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "15"), value: 15}},
              right_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "5"), value: 5}}
            }}
       }}
    ]

    # Go over every input let statement, and check it against the parsed output (identifier and value).
    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          case stmt.expression do
            {:hash, hash_literal} ->
              assert hash_literal.data == expected_output
          end
      end
    end)
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

  test "parser can parse basic infix expressions" do
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

  test "parser can parse mixed infix/prefix expressions that rely on correct precedence rules" do
    inputs = [
      "-a * b",
      "!-a",
      "a + b + c",
      "a * b + c",
      "a - b * c",
      "(a + b) * (c - 42)",
      "a * [1, 2, 3, 4][b * c] * d"
    ]

    outputs = [
      # "-a * b" should be parsed as "((-a) * b)", i.e. -a as an expression multiplied by b.
      {:infix,
       %Ast.Infix{
         operator_token: Token.init(:asterisk, "*"),
         left_expression:
           {:prefix,
            %Ast.Prefix{
              operator_token: Token.init(:minus, "-"),
              right_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}}
            }},
         right_expression:
           {:identifier, %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}}
       }},
      # "!-a" should be parsed as "(!(-a))"
      {:prefix,
       %Ast.Prefix{
         operator_token: Token.init(:bang, "!"),
         right_expression:
           {:prefix,
            %Ast.Prefix{
              operator_token: Token.init(:minus, "-"),
              right_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}}
            }}
       }},
      # "a + b + c" should be parsed as "((a + b) + c)"
      {:infix,
       %Ast.Infix{
         operator_token: Token.init(:plus, "+"),
         left_expression:
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:plus, "+"),
              left_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
              right_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}}
            }},
         right_expression:
           {:identifier, %Ast.Identifier{token: Token.init(:ident, "c"), value: "c"}}
       }},
      # "a * b + c" should be parsed as "((a * b) + c)"
      {:infix,
       %Ast.Infix{
         operator_token: Token.init(:plus, "+"),
         left_expression:
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:asterisk, "*"),
              left_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
              right_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}}
            }},
         right_expression:
           {:identifier, %Ast.Identifier{token: Token.init(:ident, "c"), value: "c"}}
       }},
      # "a - b * c" should be parsed as "(a - (b * c))"
      {:infix,
       %Ast.Infix{
         operator_token: Token.init(:minus, "-"),
         left_expression:
           {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
         right_expression:
           {:infix,
            %Ast.Infix{
              operator_token: Token.init(:asterisk, "*"),
              left_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}},
              right_expression:
                {:identifier, %Ast.Identifier{token: Token.init(:ident, "c"), value: "c"}}
            }}
       }},

      # "(a + b) * (c - 42)" should be parsed as... well you get the point, but this one has grouped expressions to specify the precedence.
      {:infix,
       %Ast.Infix{
         operator_token: Token.init(:asterisk, "*"),
         left_expression:
           {:grouped,
            %Ast.GroupedExpression{
              expression:
                {:infix,
                 %Ast.Infix{
                   operator_token: Token.init(:plus, "+"),
                   left_expression:
                     {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
                   right_expression:
                     {:identifier, %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}}
                 }}
            }},
         right_expression:
           {:grouped,
            %Ast.GroupedExpression{
              expression:
                {:infix,
                 %Ast.Infix{
                   operator_token: Token.init(:minus, "-"),
                   left_expression:
                     {:identifier, %Ast.Identifier{token: Token.init(:ident, "c"), value: "c"}},
                   right_expression:
                     {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "42"), value: 42}}
                 }}
            }}
       }},
      {
        :infix,
        %Ast.Infix{
          operator_token: Token.init(:asterisk, "*"),
          left_expression:
            {:infix,
             %Ast.Infix{
               operator_token: Token.init(:asterisk, "*"),
               left_expression:
                 {:identifier, %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
               right_expression:
                 {:index_expression,
                  %Ast.IndexExpression{
                    left:
                      {:array,
                       %Ast.ArrayLiteral{
                         token: Token.init(:lbracket, "["),
                         elements: [
                           {:integer,
                            %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}},
                           {:integer,
                            %Ast.IntegerLiteral{token: Token.init(:int, "2"), value: 2}},
                           {:integer,
                            %Ast.IntegerLiteral{token: Token.init(:int, "3"), value: 3}},
                           {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "4"), value: 4}}
                         ]
                       }},
                    index:
                      {:infix,
                       %Ast.Infix{
                         operator_token: Token.init(:asterisk, "*"),
                         left_expression:
                           {:identifier,
                            %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}},
                         right_expression:
                           {:identifier,
                            %Ast.Identifier{token: Token.init(:ident, "c"), value: "c"}}
                       }}
                  }}
             }},
          right_expression:
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "d"), value: "d"}}
        }
      }
    ]

    Enum.zip(inputs, outputs)
    |> Enum.each(fn {input, expected_output} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          assert stmt.expression == expected_output
      end
    end)
  end

  test "parser can parse if expressions" do
    inputs_and_outputs = [
      {"if (x == 42) { 25 }",
       {:if_expression,
        %Ast.IfExpression{
          condition:
            {:infix,
             %Ast.Infix{
               operator_token: Token.init(:eq, "=="),
               left_expression:
                 {:identifier, %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}},
               right_expression:
                 {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "42"), value: 42}}
             }},
          consequence:
            {:block_statement,
             %Ast.BlockStatement{
               literal: "unused",
               statements: [
                 {:expression_statement,
                  %Ast.ExpressionStatement{
                    literal: "unused",
                    expression:
                      {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "25"), value: 25}}
                  }}
               ]
             }},
          alternative: nil
        }}},
      {"if (x * 1 == y) { z * 5 } else { 0; 42 }",
       {:if_expression,
        %Ast.IfExpression{
          condition:
            {:infix,
             %Ast.Infix{
               operator_token: Token.init(:eq, "=="),
               left_expression:
                 {:infix,
                  %Ast.Infix{
                    operator_token: Token.init(:asterisk, "*"),
                    left_expression:
                      {:identifier, %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}},
                    right_expression:
                      {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}}
                  }},
               right_expression:
                 {:identifier, %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}}
             }},
          consequence:
            {:block_statement,
             %Ast.BlockStatement{
               literal: "unused",
               statements: [
                 {:expression_statement,
                  %Ast.ExpressionStatement{
                    literal: "unused",
                    expression:
                      {:infix,
                       %Ast.Infix{
                         operator_token: Token.init(:asterisk, "*"),
                         left_expression:
                           {:identifier,
                            %Ast.Identifier{token: Token.init(:ident, "z"), value: "z"}},
                         right_expression:
                           {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "5"), value: 5}}
                       }}
                  }}
               ]
             }},
          alternative:
            {:block_statement,
             %Ast.BlockStatement{
               literal: "unused",
               statements: [
                 {:expression_statement,
                  %Ast.ExpressionStatement{
                    literal: "unused",
                    expression:
                      {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "0"), value: 0}}
                  }},
                 {:expression_statement,
                  %Ast.ExpressionStatement{
                    literal: "unused",
                    expression:
                      {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "42"), value: 42}}
                  }}
               ]
             }}
        }}}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          assert stmt.expression == expected_output
      end
    end)
  end

  test "parser can parse function literal expressions" do
    inputs_and_outputs = [
      {"fn (x, y) { return x + y; }",
       {:function_literal,
        %Ast.FunctionLiteral{
          parameters: [
            %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"},
            %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}
          ],
          body:
            {:block_statement,
             %Ast.BlockStatement{
               literal: "unused",
               statements: [
                 {:return_statement,
                  %Ast.ReturnStatement{
                    literal: "unused",
                    return_value:
                      {:infix,
                       %Ast.Infix{
                         operator_token: Token.init(:plus, "+"),
                         left_expression:
                           {:identifier,
                            %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}},
                         right_expression:
                           {:identifier,
                            %Ast.Identifier{token: Token.init(:ident, "y"), value: "y"}}
                       }}
                  }}
               ]
             }}
        }}}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          assert stmt.expression == expected_output
      end
    end)
  end

  test "parser can parse function call expressions, including of function literals" do
    inputs_and_outputs = [
      {"add(1, x)",
       {:call_expression,
        %Ast.CallExpression{
          function:
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "add"), value: "add"}},
          arguments: [
            {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}},
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}}
          ]
        }}},
      {"fn (x) { } (25)",
       {:call_expression,
        %Ast.CallExpression{
          function:
            {:function_literal,
             %Ast.FunctionLiteral{
               parameters: [%Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}],
               body: {:block_statement, %Ast.BlockStatement{literal: "unused", statements: []}}
             }},
          arguments: [
            {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "25"), value: 25}}
          ]
        }}},
      {"my_func(x, fn(a, b){return a - b;})",
       {:call_expression,
        %Ast.CallExpression{
          function:
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "my_func"), value: "my_func"}},
          arguments: [
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "x"), value: "x"}},
            {:function_literal,
             %Ast.FunctionLiteral{
               parameters: [
                 %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"},
                 %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}
               ],
               body:
                 {:block_statement,
                  %Ast.BlockStatement{
                    literal: "unused",
                    statements: [
                      {:return_statement,
                       %Ast.ReturnStatement{
                         literal: "unused",
                         return_value:
                           {:infix,
                            %Ast.Infix{
                              operator_token: Token.init(:minus, "-"),
                              left_expression:
                                {:identifier,
                                 %Ast.Identifier{token: Token.init(:ident, "a"), value: "a"}},
                              right_expression:
                                {:identifier,
                                 %Ast.Identifier{token: Token.init(:ident, "b"), value: "b"}}
                            }}
                       }}
                    ]
                  }}
             }}
          ]
        }}}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          assert stmt.expression == expected_output
      end
    end)
  end

  test "parser can parse index expressions" do
    inputs_and_outputs = [
      {"myArray[1 + 1]",
       {:index_expression,
        %Ast.IndexExpression{
          left:
            {:identifier, %Ast.Identifier{token: Token.init(:ident, "myArray"), value: "myArray"}},
          index: {
            :infix,
            %Ast.Infix{
              operator_token: Token.init(:plus, "+"),
              left_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}},
              right_expression:
                {:integer, %Ast.IntegerLiteral{token: Token.init(:int, "1"), value: 1}}
            }
          }
        }}}
    ]

    inputs_and_outputs
    |> Enum.each(fn {input, expected_output} ->
      program =
        input
        |> Lexer.init()
        |> Parser.init()
        |> Parser.parse_program()

      assert length(program.statements) == 1

      case program.statements |> List.first() do
        {:expression_statement, stmt} ->
          assert stmt.expression == expected_output
      end
    end)
  end
end
