# DEPENDENCIES
# xclip, nvim, git, tree, starship

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES ------------------------------------------------------------------
alias vim="nvim"

alias gits='git status'
alias gitl='git log -n 5'
alias gitdiff='git diff . | bat'

alias ll='ls -lA'
alias cll='clear; ls -lA'

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

# --- Starship Prompt ----------------------------------------------------------
starship init fish | source

# --- Remove the greetings message ---------------------------------------------
set -U fish_greeting

# --- ENV ----------------------------------------------------------------------
set EDITOR /usr/bin/nvim
set DOTNET_CLI_TELEMETRY_OPTOUT 0
set DOTNET_ROOT $HOME/software/dotnet 
set DOTNET_TOOLS_HOME $HOME/.dotnet/tools
set PATH $BASE_PATH $DOTNET_ROOT $DOTNET_TOOLS_HOME
