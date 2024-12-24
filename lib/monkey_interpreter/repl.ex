defmodule MonkeyInterpreter.Repl do
  alias MonkeyInterpreter.{Lexer, Parser}

  @spec start() :: no_return()
  def start do
    username = get_username()

    IO.puts(
      "Hello, #{username}! This is the Monkey programming language!\nFeel free to type in commands\n"
    )

    read_loop()
  end

  @spec read_loop() :: no_return()
  def read_loop() do
    input = IO.gets(">> ")

    case input do
      :eof ->
        IO.puts("Got to EOF, shutting down...")
        System.stop(0)
        :ok

      {:error, reason} ->
        IO.puts("Error: #{reason}")

      # Display the parsed AST, then recurse infinitely.
      input ->
        program = input |> Lexer.init() |> Parser.init() |> Parser.parse_program()

        program.statements |> Enum.each(&IO.inspect(&1))

        read_loop()
    end
  end

  defp get_username do
    System.get_env("USER") || System.get_env("USERNAME")
  end
end
