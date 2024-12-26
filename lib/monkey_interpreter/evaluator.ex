defmodule MonkeyInterpreter.Evaluator do
  alias MonkeyInterpreter.{Ast, Token}

  @spec evaluate(Ast.Program.t()) :: {:ok, any()} | {:error, String.t()}
  def evaluate(%Ast.Program{} = program) do
    # Eval all the statements (stop early after you encounter a :return_statement) and return the value of the last one.
    program.statements
    |> Enum.reduce_while(nil, fn
      stmt, _acc ->
        case eval(stmt) do
          {:ok, value} -> {:cont, {:ok, value}}
          {:error, reason} -> {:halt, {:error, reason}}
          {:returned, value} -> {:halt, {:ok, value}}
        end
    end)
  end

  # The :returned atom indicates this value was returned as part of a return statement, so subsequent statements should not be evaluated and we should return "early".
  @spec eval(Ast.Node.t()) :: {:ok | :returned, any()} | {:error, String.t()}

  defp eval({:let_statement, %Ast.LetStatement{} = stmt}) do
    # Evaluate the right-hand side.
    case eval(stmt.assigned_value) do
      {:error, reason} -> {:error, reason}
      # TODO do the assignment and stuff
      # TODO The statement itself returns nothing, I think.
      {:ok, _value} -> {:ok, nil}
    end
  end

  defp eval({:return_statement, %Ast.ReturnStatement{} = stmt}) do
    case eval(stmt.return_value) do
      {:error, reason} -> {:error, reason}
      {_atom, value} -> {:returned, value}
    end
  end

  defp eval({:expression_statement, %Ast.ExpressionStatement{} = stmt}) do
    eval(stmt.expression)
  end

  defp eval({:block_statement, %Ast.BlockStatement{} = stmt}) do
    # Eval all the statements (stop early after you encounter a :return_statement) and return the value of the last one.
    stmt.statements
    |> Enum.reduce_while(nil, fn
      stmt, _acc ->
        case eval(stmt) do
          {:ok, _} = result -> {:cont, result}
          {:error, _} = result -> {:halt, result}
          {:returned, _} = result -> {:halt, result}
        end
    end)
  end

  defp eval({:identifier, %Ast.Identifier{token: _token, value: value}}) do
    # TODO return the actual value stored in the identifier
    {:ok, value}
  end

  defp eval({:boolean, %Ast.Boolean{value: value}}) do
    {:ok, value}
  end

  defp eval({:integer, %Ast.IntegerLiteral{value: value}}) do
    {:ok, value}
  end

  defp eval({:grouped, %Ast.GroupedExpression{expression: expression}}) do
    eval(expression)
  end

  defp eval({:if_expression, %Ast.IfExpression{} = expr}) do
    # Evaluate the condition, and if found truthy, evaluate and return the consequence. Otherwise, use the alternative.
    case eval(expr.condition) do
      # Early return if error.
      {:error, reason} ->
        {:error, reason}

      {:ok, condition_result} ->
        clause =
          if Token.is_truthy(condition_result), do: expr.consequence, else: expr.alternative

        # If the alternative is chosen but it doesn't exist, return nil.
        if clause != nil, do: eval(clause), else: {:ok, nil}
    end
  end

  defp eval({:function_literal, %Ast.FunctionLiteral{} = _expr}) do
    # TODO this is just an anonymous function, there's nothing to "eval"...
    raise("Function literal unimplemented")
  end

  defp eval({:call_expression, %Ast.CallExpression{} = _expr}) do
    # TODO if function refers to a callable, then make the call. Otherwise, throw an error.
    raise("Call expression unimplemented")
  end

  defp eval({:prefix, %Ast.Prefix{operator_token: token} = expr}) do
    if Token.is_prefix(token) do
      case eval(expr.right_expression) do
        # Early return if error.
        {:error, reason} ->
          {:error, reason}

        {:ok, value} ->
          # TODO add type validation (e.g. can't use "-" on a bool)
          operator = Token.to_prefix_operator(token)
          operator.(value)
      end
    else
      raise("Invalid prefix operator: #{inspect(token)}")
    end
  end

  defp eval({:infix, %Ast.Infix{} = expr}) do
    if Token.is_infix(expr.operator_token) do
      case eval(expr.left_expression) do
        {:error, reason} ->
          {:error, reason}

        {:ok, left} ->
          case eval(expr.right_expression) do
            {:error, reason} ->
              {:error, reason}

            {:ok, right} ->
              # TODO add type validation (e.g. can't use "-" on bools)
              operator = Token.to_infix_operator(expr.operator_token)
              operator.(left, right)
          end
      end
    else
      raise("Invalid infix operator: #{inspect(expr.operator_token)}")
    end
  end

  defp eval(x) do
    raise("Unimplemented eval: #{inspect(x)}")
  end
end
