# DEPENDENCIES
# xclip, nvim, git, tree, starship

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES ------------------------------------------------------------------

alias vim='nvim'

alias hx='helix'

alias gits='git status'
alias gitl='git log -n 5'
alias gitd='git diff . | bat'
alias gitp='git push'

alias ls='ls --color=auto'
alias ll='ls -lAF'
alias cll='clear; ls -lAF'
alias ..='cd ..'

alias dnb="dotnet build"
alias dnr="dotnet run"
alias dnwr="dotnet watch run"
alias dnt='clear && dotnet test'

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

alias du-here='du -h -d 1 | sort -hr | head --lines 20'

alias tar-to='tar -xzvf '

# --- Starship Prompt ----------------------------------------------------------

starship init fish | source

# --- Remove the greetings message ---------------------------------------------

set -U fish_greeting

# --- ENV ----------------------------------------------------------------------

# set SHELL /usr/bin/fish
# set EDITOR /usr/bin/nvim

# set DOTNET_ROOT $HOME/software/dotnet6.0.0
# set DOTNET_TOOLS_HOME $HOME/.dotnet/tools
# set PATH $PATH $DOTNET_ROOT $DOTNET_TOOLS_HOME

set DOTNET_CLI_TELEMETRY_OPTOUT 1 # 1 to refuse
