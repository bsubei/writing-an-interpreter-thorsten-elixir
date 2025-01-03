defmodule MonkeyInterpreter.Evaluator do
  alias MonkeyInterpreter.{Ast, Token, Object, Function, Environment, Builtin, Array, Hash}

  @spec evaluate(Ast.Program.t(), Environment.t()) ::
          {:ok, Object.t(), Environment.t()} | {:error, String.t()}
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
  @spec eval(Ast.Node.t(), Environment.t()) ::
          {:ok | :returned, Object.t(), Environment.t()} | {:error, String.t()}

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
    |> Enum.reduce_while({:ok, nil, environment}, fn
      stmt, {:ok, _value, environment} ->
        case eval(stmt, environment) do
          {:ok, _, _} = result -> {:cont, result}
          {:error, _} = result -> {:halt, result}
          {:returned, _, _} = result -> {:halt, result}
        end
    end)
  end

  defp eval({:identifier, %Ast.Identifier{value: identifier_name}} = input, environment) do
    # Look up the value of the identifier in the current environment. If not found, look in the outer environments recursively.
    case Map.get(environment.bindings, identifier_name) do
      nil when environment.outer_environment == nil ->
        # Once we're in the outermost environment, check for builtin functions, otherwise we're out of places to search so error out.
        case Builtin.builtins() |> Map.get(identifier_name) do
          nil -> {:error, "identifier not found: #{identifier_name}"}
          builtin -> {:ok, builtin, environment}
        end

      nil ->
        # NOTE: the outer_environment shouldn't be returned, instead the original environment should be returned.
        case eval(input, environment.outer_environment) do
          {:error, reason} -> {:error, reason}
          {:ok, value, _ignored_env} -> {:ok, value, environment}
        end

      value ->
        {:ok, value, environment}
    end
  end

  defp eval({:boolean, %Ast.Boolean{value: value}}, environment) do
    {:ok, value, environment}
  end

  defp eval({:integer, %Ast.IntegerLiteral{value: value}}, environment) do
    {:ok, value, environment}
  end

  defp eval({:string, %Ast.StringLiteral{value: value}}, environment) do
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

  defp eval({:array, %Ast.ArrayLiteral{} = expr}, environment) do
    # Eval and collect all the expressions in the array literal (stop early after you encounter an :error).
    case expr.elements
         |> Enum.reduce_while([], fn
           expression, acc ->
             case eval(expression, environment) do
               {:ok, object, _env} -> {:cont, acc ++ [object]}
               {:error, reason} -> {:halt, {:error, reason}}
             end
         end) do
      {:error, reason} -> {:error, reason}
      objects -> {:ok, %Array{elements: objects}, environment}
    end
  end

  defp eval({:function_literal, %Ast.FunctionLiteral{} = expr}, environment) do
    # Store the enclosing environment so it can be used when the function is applied/invoked, i.e. make it a closure.
    function = %Function{parameters: expr.parameters, body: expr.body, environment: environment}
    {:ok, function, environment}
  end

  defp eval({:hash, %Ast.HashLiteral{} = expr}, environment) do
    # Eval and collect all the key-value expressions in the hash literal (stop early after you encounter an :error).
    case expr.data
         |> Enum.reduce_while(%{}, fn
           {key_expression, value_expression}, acc ->
             case eval(key_expression, environment) do
               {:ok, key, _env} ->
                 case eval(value_expression, environment) do
                   {:ok, value, _env} -> {:cont, Map.put(acc, key, value)}
                   {:error, reason} -> {:halt, {:error, reason}}
                 end

               {:error, reason} ->
                 {:halt, {:error, reason}}
             end
         end) do
      {:error, reason} -> {:error, reason}
      data -> {:ok, %Hash{data: data}, environment}
    end
  end

  defp eval({:call_expression, %Ast.CallExpression{} = expr}, environment) do
    # Evaluate the function identifier or literal first.
    case eval(expr.function, environment) do
      {:error, reason} ->
        {:error, reason}

      {:ok, function, environment} ->
        # Then evaluate the arguments.
        case eval_args(expr.arguments, environment) do
          {:error, reason} ->
            {:error, reason}

          {:ok, args, environment} ->
            case function do
              %Function{} ->
                apply_function(function, args, environment)

              %Builtin{func: function} ->
                case function.(args) do
                  {:error, reason} -> {:error, reason}
                  {:ok, value} -> {:ok, value, environment}
                end

              unknown_expr ->
                {:error, "expected function literal or identifier, got: #{unknown_expr}"}
            end
        end

      {:returned, expr, _env} ->
        {:error, "did not expect a :returned expression: #{expr}"}
    end
  end

  defp eval({:index_expression, %Ast.IndexExpression{} = expr}, environment) do
    case eval(expr.left, environment) do
      {:error, reason} ->
        {:error, reason}

      # TODO simplify by matching on object earlier (?)
      {:ok, object, _env} ->
        case eval(expr.index, environment) do
          {:error, reason} ->
            {:error, reason}

          {:ok, index, _env} ->
            case({object, index}) do
              {%Array{}, index} when is_integer(index) and index < 0 ->
                {:ok, nil, environment}

              {%Array{elements: elements}, index} when is_integer(index) and index >= 0 ->
                {value, _new_array} = elements |> List.pop_at(index)
                {:ok, value, environment}

              {%Hash{data: data}, key}
              when is_integer(key) or is_binary(key) or is_boolean(key) ->
                value = data |> Map.get(index)
                {:ok, value, environment}

              {%Hash{}, _key} ->
                {:error, "unusable as a hash key: #{Token.user_displayed_type(index)}"}

              _ ->
                {:error,
                 "Index operator requires an array or hash, got: #{Token.user_displayed_type(object)} and index: #{Token.user_displayed_type(index)}"}
            end
        end
    end
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
      {:error, "Invalid prefix operator: #{inspect(token)}"}
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
      {:error, "Invalid prefix operator: #{inspect(expr.operator_token)}"}
    end
  end

  defp eval(x, _environment) do
    {:error, "Unimplemented eval for: #{inspect(x)}"}
  end

  @spec eval_args(list(Ast.Expression.t()), Environment.t(), list(Object.t())) ::
          {:ok, list(Object.t()), Environment.t()} | {:error, String.t()}
  defp eval_args(args, environment, acc \\ [])

  defp eval_args([], environment, acc), do: {:ok, acc, environment}

  defp eval_args([arg | rest], environment, acc) do
    case(eval(arg, environment)) do
      {:error, reason} -> {:error, reason}
      {:ok, value, _env} -> eval_args(rest, environment, acc ++ [value])
    end
  end

  @spec apply_function(Function.t(), list(Object.t()), Environment.t()) ::
          {:ok, Object.t(), Environment.t()} | {:error, String.t()}
  defp apply_function(function, args, caller_environment) do
    # Create a new inner environment that contains the function parameters as bindings to the arguments.
    new_bindings =
      Enum.zip(function.parameters, args)
      |> Enum.map(fn {param, arg} -> {param.value, arg} end)
      |> Enum.into(%{})

    new_environment = %Environment{bindings: new_bindings, outer_environment: caller_environment}

    # Do not bubble up :returned. Also, discard the inner environment after using it.
    case eval(function.body, new_environment) do
      {:error, reason} -> {:error, reason}
      {:returned, value, _env} -> {:ok, value, caller_environment}
      {:ok, value, _env} -> {:ok, value, caller_environment}
    end
  end
end
