
export PATH="$HOME/.elan/bin:$PATH"

export EDITOR="vim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export READER="zathura"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export LESSHISTFILE="-"
export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/history"
export GEM_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/gem"
export GEM_SPEC_CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/gem"
export RUSTUP_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/rustup"
export STACK_ROOT="${XDG_DATA_HOME:-$HOME/.local/share}/stack"
