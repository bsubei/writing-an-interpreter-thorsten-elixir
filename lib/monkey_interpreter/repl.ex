defmodule MonkeyInterpreter.Repl do
  alias MonkeyInterpreter.Lexer

  @spec start() :: no_return()
  def start do
    username = "TODO"

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

      # Display the lexed input, then recurse infinitely.
      input ->
        lexer = Lexer.init(input)

        Lexer.all_tokens(lexer) |> Enum.map(fn token -> IO.puts(inspect(token)) end)

        read_loop()
    end
  end
end
