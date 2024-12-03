# Use the official Elixir image from Docker Hub
FROM elixir:latest

# Install basic development tools
RUN apt-get update && apt-get install -y \
    git \
    curl \
    wget \
    inotify-tools \
    && rm -rf /var/lib/apt/lists/*

# Configure Git to trust the workspace
RUN git config --global --add safe.directory /workspaces/elixir-devcontainer \
&& git config --global --add safe.directory /workspace

# Set working directory
WORKDIR /workspace

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force
