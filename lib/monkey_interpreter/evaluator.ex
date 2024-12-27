defmodule MonkeyInterpreter.Evaluator do
  alias MonkeyInterpreter.{Ast, Token}

  @spec evaluate(Ast.Program.t(), Ast.Environment.t()) ::
          {:ok, any(), Ast.Environment.t()} | {:error, String.t()}
  def evaluate(%Ast.Program{} = program, environment) do
    # Eval all the statements (stop early after you encounter a :return_statement) and return the value of the last one.
    program.statements
    |> Enum.reduce_while({:ok, nil, environment}, fn
      stmt, {:ok, _value, environment} ->
        case eval(stmt, environment) do
          {:ok, value, environment} -> {:cont, {:ok, value, environment}}
          {:error, reason} -> {:halt, {:error, reason}}
          {:returned, value, environment} -> {:halt, {:ok, value, environment}}
        end
    end)
  end

  # The :returned atom indicates this value was returned as part of a return statement, so subsequent statements should not be evaluated and we should return "early".
  @spec eval(Ast.Node.t(), Ast.Environment.t()) ::
          {:ok | :returned, any(), Ast.Environment.t()} | {:error, String.t()}

  defp eval({:let_statement, %Ast.LetStatement{} = stmt}, environment) do
    # Evaluate the right-hand side.
    case eval(stmt.assigned_value, environment) do
      {:error, reason} ->
        {:error, reason}

      {:ok, value, environment} ->
        # Extend the environment to add this new binding.
        new_environment =
          update_in(environment.bindings, &Map.put(&1, stmt.identifier.value, value))

        # The statement itself returns nothing, but the environment is updated.
        {:ok, nil, new_environment}
    end
  end

  defp eval({:return_statement, %Ast.ReturnStatement{} = stmt}, environment) do
    case eval(stmt.return_value, environment) do
      {:error, reason} -> {:error, reason}
      {_atom, value, environment} -> {:returned, value, environment}
    end
  end

  defp eval({:expression_statement, %Ast.ExpressionStatement{} = stmt}, environment) do
    eval(stmt.expression, environment)
  end

  defp eval({:block_statement, %Ast.BlockStatement{} = stmt}, environment) do
    # Eval all the statements (stop early after you encounter a :return_statement) and return the value of the last one.
    stmt.statements
    |> Enum.reduce_while(nil, fn
      stmt, _acc ->
        case eval(stmt, environment) do
          {:ok, _, _} = result -> {:cont, result}
          {:error, _} = result -> {:halt, result}
          {:returned, _, _} = result -> {:halt, result}
        end
    end)
  end

  defp eval({:identifier, %Ast.Identifier{value: value}}, environment) do
    # TODO look up the value in the current environment (and if not found, in the outer environments recursively).
    case Map.get(environment.bindings, value) do
      nil -> {:error, "identifier not found: #{value}"}
      value -> {:ok, value, environment}
    end
  end

  defp eval({:boolean, %Ast.Boolean{value: value}}, environment) do
    {:ok, value, environment}
  end

  defp eval({:integer, %Ast.IntegerLiteral{value: value}}, environment) do
    {:ok, value, environment}
  end

  defp eval({:grouped, %Ast.GroupedExpression{expression: expression}}, environment) do
    eval(expression, environment)
  end

  defp eval({:if_expression, %Ast.IfExpression{} = expr}, environment) do
    # Evaluate the condition, and if found truthy, evaluate and return the consequence. Otherwise, use the alternative.
    case eval(expr.condition, environment) do
      # Early return if error.
      {:error, reason} ->
        {:error, reason}

      {:ok, condition_result, environment} ->
        clause =
          if Token.is_truthy(condition_result), do: expr.consequence, else: expr.alternative

        # If the alternative is chosen but it doesn't exist, return nil.
        if clause != nil, do: eval(clause, environment), else: {:ok, nil, environment}
    end
  end

  defp eval({:function_literal, %Ast.FunctionLiteral{} = _expr}, _environment) do
    # TODO this is just an anonymous function, there's nothing to "eval"...
    raise("Function literal unimplemented")
  end

  defp eval({:call_expression, %Ast.CallExpression{} = _expr}, _environment) do
    # TODO if function refers to a callable, then make the call. Otherwise, throw an error.
    raise("Call expression unimplemented")
  end

  defp eval({:prefix, %Ast.Prefix{operator_token: token} = expr}, environment) do
    if Token.is_prefix(token) do
      case eval(expr.right_expression, environment) do
        # Early return if error.
        {:error, reason} ->
          {:error, reason}

        {:ok, value, environment} ->
          operator = Token.to_prefix_operator(token)

          case operator.(value) do
            {:ok, value} -> {:ok, value, environment}
            {:error, reason} -> {:error, reason}
          end
      end
    else
      raise("Invalid prefix operator: #{inspect(token)}")
    end
  end

  defp eval({:infix, %Ast.Infix{} = expr}, environment) do
    if Token.is_infix(expr.operator_token) do
      case eval(expr.left_expression, environment) do
        {:error, reason} ->
          {:error, reason}

        {:ok, left, environment} ->
          case eval(expr.right_expression, environment) do
            {:error, reason} ->
              {:error, reason}

            {:ok, right, environment} ->
              operator = Token.to_infix_operator(expr.operator_token)

              case operator.(left, right) do
                {:ok, value} -> {:ok, value, environment}
                {:error, reason} -> {:error, reason}
              end
          end
      end
    else
      raise("Invalid infix operator: #{inspect(expr.operator_token)}")
    end
  end

  defp eval(x, _environment) do
    raise("Unimplemented eval: #{inspect(x)}")
  end
end
