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
alias ls='ls --color=auto'
alias ll='ls -lAF'
alias vim='nvim'
alias gvim='nvim-qt'
alias gits='git status'

# Startship Prompt
eval "$(starship init zsh)"
