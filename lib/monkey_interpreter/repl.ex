defmodule MonkeyInterpreter.Repl do
  alias MonkeyInterpreter.{Lexer, Parser, Evaluator, Environment}

  @spec start() :: no_return()
  def start do
    username = get_username()

    IO.puts(
      "Hello, #{username}! This is the Monkey programming language!\nFeel free to type in commands\n"
    )

    read_loop(Environment.init())
  end

  @spec read_loop(Environment.t()) :: no_return()
  def read_loop(environment) do
    input = IO.gets(">> ")

    case input do
      :eof ->
        IO.puts("Got to EOF, shutting down...")
        System.stop(0)
        :ok

      {:error, reason} ->
        IO.puts("Error: #{reason}")

      # Evaluate the input, display it, then recurse infinitely. Make sure to pass in the updated environment.
      input ->
        environment =
          case input
               |> Lexer.init()
               |> Parser.init()
               |> Parser.parse_program()
               |> Evaluator.evaluate(environment) do
            {:error, reason} ->
              IO.puts(:stderr, "ERROR: #{reason}")
              environment

            {:ok, value, new_environment} ->
              IO.puts(value)
              new_environment
          end

        read_loop(environment)
    end
  end

  defp get_username do
    # I couldn't find a better way to get the username after a quick search.
    :init.get_argument(:home)
    |> elem(1)
    |> List.first()
    |> Path.basename()
  end
end
