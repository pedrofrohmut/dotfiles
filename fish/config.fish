# DEPENDENCIES
# xclip, nvim, git, tree, starship

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES ------------------------------------------------------------------
alias vim="nvim"

alias gits='git status'
alias gitl='git log -n 5'
alias gitd='git diff . | bat'

alias ls='ls --color=auto'
alias ll='ls -lA'
alias cll='clear; ls -lA'
alias ..='cd ..'

alias dnb="dotnet build"
alias dnr="dotnet run"
alias dnwr="dotnet watch run"

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

# --- Starship Prompt ----------------------------------------------------------
starship init fish | source

# --- Remove the greetings message ---------------------------------------------
set -U fish_greeting

# --- ENV ----------------------------------------------------------------------
set SHELL /usr/bin/fish
set EDITOR /usr/bin/nvim
set DOTNET_CLI_TELEMETRY_OPTOUT 0
set DOTNET_ROOT $HOME/software/dotnet6.0.0 
set DOTNET_TOOLS_HOME $HOME/.dotnet/tools
set PATH $PATH $DOTNET_ROOT $DOTNET_TOOLS_HOME
