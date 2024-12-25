defmodule MonkeyInterpreter.Evaluator do
  alias MonkeyInterpreter.{Ast, TokenType}

  @spec eval(Ast.node_t()) :: any()

  def eval(%Ast.Program{} = program) do
    # Loop over each statement and eval it, and return the value of the last one.
    program.statements |> Enum.map(fn stmt -> eval(stmt) end) |> List.last()
  end

  def eval({:let_statement, %Ast.LetStatement{} = stmt}) do
    # Evaluate the right-hand side.
    eval(stmt.assigned_value)
    # TODO do the assignment and stuff
    # The statement itself returns nothing.
    nil
  end

  def eval({:return_statement, %Ast.ReturnStatement{} = stmt}) do
    # TODO do something with the value to return (actually "return" it)
    eval(stmt.return_value)
    # The statement itself doesn't return anything (?).
    nil
  end

  def eval({:expression_statement, %Ast.ExpressionStatement{} = stmt}) do
    eval(stmt.expression)
    # TODO The statement itself doesn't return anything. Or does it?
  end

  def eval({:block_statement, %Ast.BlockStatement{} = stmt}) do
    # Evaluate each statement in series.
    stmt.statements |> Enum.each(fn stmt -> eval(stmt) end)
    # The statement itself doesn't return anything.
    nil
  end

  def eval({:identifier, %Ast.Identifier{token: _token, value: value}}) do
    # TODO return the actual value stored in the identifier
    value
  end

  def eval({:boolean, %Ast.Boolean{value: value}}) do
    value
  end

  def eval({:integer, %Ast.IntegerLiteral{value: value}}) do
    value
  end

  def eval({:grouped, %Ast.GroupedExpression{expression: expression}}) do
    eval(expression)
  end

  def eval({:if, %Ast.IfExpression{} = expr}) do
    # Evaluate the condition, and if found truthy, evaluate and return the consequence. Otherwise, use the alternative.
    clause = if is_truthy(eval(expr.condition)), do: expr.consequence, else: expr.alternative
    eval(clause)
  end

  def eval({:function_literal, %Ast.FunctionLiteral{} = _expr}) do
    # TODO this is just an anonymous function, there's nothing to "eval"...
    raise("Function literal unimplemented")
  end

  def eval({:call_expression, %Ast.CallExpression{} = _expr}) do
    # TODO if function refers to a callable, then make the call. Otherwise, throw an error.
    raise("Call expression unimplemented")
  end

  def eval({:prefix, %Ast.Prefix{operator_token: token} = expr}) do
    # There are only two prefix operators: "!" and "-"
    if TokenType.is_prefix(token.type) do
      value = eval(expr.right_expression)
      operator = TokenType.to_prefix_operator(token.type)
      operator.(value)
    else
      raise("Invalid prefix operator: #{inspect(token)}")
    end
  end

  def eval({:infix, %Ast.Infix{} = expr}) do
    if TokenType.is_infix(expr.operator_token.type) do
      left = eval(expr.left_expression)
      right = eval(expr.right_expression)
      operator = TokenType.to_infix_operator(expr.operator_token.type)
      operator.(left, right)
    else
      raise("Invalid infix operator: #{inspect(expr.operator_token)}")
    end
  end

  def eval(x) do
    raise("Unimplemented eval: #{inspect(x)}")
  end

  # The definition of truthy/falsey in the Monkey language differs from Elixir (the host language).
  defp is_truthy(value) do
    # TODO actually implement
    if value do
      true
    else
      false
    end
  end
end
