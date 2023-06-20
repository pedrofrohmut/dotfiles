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
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim

# export DOTNET_ROOT=$HOME/software/dotnet7.0.203
# export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
# export PATH=$PATH:$DOTNET_ROOT:$DOTNET_TOOLS_HOME

export DOTNET_CLI_TELEMETRY_OPTOUT=1 # 1 to refuse

export RUST_BIN=$HOME/.cargo/bin

export LOCAL_BIN=$HOME/.local/bin # For endeavourOS

export PATH=$PATH:$RUST_BIN:$LOCAL_BIN


# --- Keybinds -----------------------------------------------------------------

bindkey -v # Enables vi mode

# \e, \E, = Escape
# ^[      = Alt key (on some keyboards this is the same as escape)
# ^?      = Delete
# ^X, ^   = Control

bindkey -e

bindkey '^[[H'  beginning-of-line                    # Home key (xterm)
bindkey '^[[OH' beginning-of-line                    # Home key (smkx mode)
bindkey '^[[1~' beginning-of-line                    # Home key (screen & tmux)
bindkey '^[[7~' beginning-of-line                    # Home key (urxvt)
bindkey '^[[F'  end-of-line                          # End key (xterm)
bindkey '^[[OF' end-of-line                          # End key (smkx mode)
bindkey '^[[4~' end-of-line                          # End key (screen & tmux)
bindkey '^[[8~' end-of-line                          # End key (urxvt)
bindkey '^[[2~' overwrite-mode                       # Insert key
bindkey '^[[3~' delete-char                          # Delete key
bindkey '^[[C'  forward-char                         # Right key
bindkey '^[[D'  backward-char                        # Left key
#bindkey '^[[5~' history-beginning-search-backward   # Page up key
#bindkey '^[[6~' history-beginning-search-forward    # Page down key
bindkey '^[[A'  up-line-or-history                   # Up key
bindkey '^[[B'  down-line-or-history                 # Down key

# Navigate words with ctrl+arrow keys
bindkey '^[Oc'    forward-word                       #
bindkey '^[Od'    backward-word                      #
bindkey '^[[1;5D' backward-word                      #
bindkey '^[[1;5C' forward-word                       #
bindkey '^H'      backward-kill-word                 # delete previous word with ctrl+backspace
bindkey '^[[Z'    undo                               # Shift+tab undo last action

# Removing keys
bindkey -r '^J' # accept-line
bindkey -r '^K' # kill-line

# --- Completion ---------------------------------------------------------------

zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' # Case insensitive tab completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' rehash true                              # automatically find new executables in path

# --- History ------------------------------------------------------------------

HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500

# --- Options ------------------------------------------------------------------

setopt nobeep                                                   # No beep

# --- Color Man Pages ----------------------------------------------------------

export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R
