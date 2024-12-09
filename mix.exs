defmodule MonkeyInterpreter.MixProject do
  use Mix.Project

  def project do
    [
      app: :monkey_interpreter,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        plt_add_deps: :apps_direct,
        plt_add_apps: [:mix, :ex_unit]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {MonkeyInterpreter.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:dialyxir, "~> 1.4", only: [:dev], runtime: false}
    ]
  end
end
