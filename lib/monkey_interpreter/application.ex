defmodule MonkeyInterpreter.Application do
  use Application

  def start(_type, _args) do
    # The only child is the REPL task, which recurses infinitely.
    children = [
      {Task, fn -> MonkeyInterpreter.Repl.start() end}
    ]

    opts = [strategy: :one_for_one, name: MonkeyInterpreter.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
