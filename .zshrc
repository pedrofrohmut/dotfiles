# Use powerline
#USE_POWERLINE="true"

# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi

# Use manjaro zsh prompt
# if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
#   source /usr/share/zsh/manjaro-zsh-prompt
# fi

# Aliases
alias vim="nvim"

alias gits='git status'
alias gitl='git log -n 5'
alias gitdiff='git diff . | bat'

alias ll='ls -lA'
alias cll='clear; ls -lA'

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

# Startship Prompt
eval "$(starship init zsh)"

# --- ENV ----------------------------------------------------------------------
export EDITOR=/usr/bin/nvim
export DOTNET_ROOT=$HOME/dotnet
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export DOTNET_CLI_TELEMETRY_OPTOUT=0
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_TOOLS_HOME
