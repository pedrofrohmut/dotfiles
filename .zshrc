# --- Aliases ------------------------------------------------------------------
alias vim='nvim'

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
alias dnt="clear && dotnet test"

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

alias pretty-json='python -m json.tool'

alias pdf="qpdfview"

# --- Starship Prompt ----------------------------------------------------------
eval "$(starship init zsh)"

# --- ENV ----------------------------------------------------------------------
export SHELL=/usr/bin/zsh
export EDITOR=/usr/bin/nvim
export DOTNET_ROOT=$HOME/software/dotnet6.0.0
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export DOTNET_CLI_TELEMETRY_OPTOUT=0
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_TOOLS_HOME
