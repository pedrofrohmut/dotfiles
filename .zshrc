# --- Aliases ------------------------------------------------------------------

alias vim='nvim'

alias hx='helix'

alias gits='git status'
alias gitl='git log -n 5'
alias gitd='git diff . | bat'
alias gitdvim='git diff . | nvim -R'
alias gitp='git push'

alias ls='ls --color=auto'
alias ll='ls -lAF'
alias cll='clear; ls -lAF'
alias ..='cd ..'

alias dnb='dotnet build'
alias dnr='dotnet run'
alias dnwr='dotnet watch run'
alias dnt='clear && dotnet test'

alias tree='tree -C'

alias jj='jobs'

alias c-path='pwd | xclip -selection clipboard'

alias pretty-json='python -m json.tool'

alias du-here='du -h -d 1 | sort -hr | head --lines 20'

alias tar-to='tar -xzvf '

alias last-installed='cat /var/log/pacman.log | grep "installed" | tail -n 10'

alias update-grub='grub-mkconfig -o /boot/grub/grub.cfg'

# --- Starship Prompt ----------------------------------------------------------

eval "$(starship init zsh)"

# --- ENV ----------------------------------------------------------------------
# export SHELL=/usr/bin/zsh
# export EDITOR=/usr/bin/nvim

# export DOTNET_ROOT=$HOME/software/dotnet7.0.203
# export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
# export PATH=$PATH:$DOTNET_ROOT:$DOTNET_TOOLS_HOME

export DOTNET_CLI_TELEMETRY_OPTOUT=1 # 1 to refuse

export RUST_BIN=$HOME/.cargo/bin

export LOCAL_BIN=$HOME/.local/bin

export PATH=$PATH:$RUST_BIN:$LOCAL_BIN
