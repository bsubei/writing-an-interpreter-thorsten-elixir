# Use the official Elixir image from Docker Hub
FROM elixir:latest

# Install basic development tools
RUN apt-get update && apt-get install -y \
    git \
    curl \
    wget \
    inotify-tools \
    less \
    readline-common \
    libreadline8 \
    && rm -rf /var/lib/apt/lists/*

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Configure Git to trust the workspace
RUN git config --global --add safe.directory /workspaces/elixir-devcontainer \
&& git config --global --add safe.directory /workspace

# Configure Git to use less as pager with proper settings
RUN git config --global core.pager 'less -R' && \
    git config --global pager.diff 'less -R' && \
    git config --global pager.show 'less -R'

# Configure less to use vim-style navigation
ENV LESS="-R -F -X --mouse --wheel-lines=3"
ENV LESSOPEN="|lesspipe.sh %s"

# Copy configuration files
COPY .bashrc /root/.bashrc
COPY .inputrc /root/.inputrc


# Set working directory
WORKDIR /workspace
