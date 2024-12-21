FROM hexpm/elixir:1.18.0-erlang-27.2-debian-bullseye-20241202-slim

# Install dependencies
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive && apt-get -y install \
    git \
    wget \
    less \
    curl \
    gnupg \
    inotify-tools \
    readline-common \
    libreadline8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Configure Git to trust all workspaces
RUN git config --global --add safe.directory '*'

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