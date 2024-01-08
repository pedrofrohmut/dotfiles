# --- Aliases ------------------------------------------------------------------

alias vim='nvim'

alias hx='helix'

alias gits='git status'
alias gitl='git log -n 5 | bat'
alias gitd='git diff . | bat'
alias gitdvim='git diff . | nvim -R'
alias gitp='git push'
alias gitac='git add . && git commit -m'

alias ls='ls --color=auto'
alias ll='ls -lAF'
alias cll='clear; ls -lAF'
alias ..='cd ..'
alias cp='cp --verbose'

alias dnb='dotnet build'
alias dnr='dotnet run'
alias dnwr='dotnet watch run'
alias dnt='clear && dotnet test'

alias tree='tree -C'

alias jj='jobs'

alias c-path='pwd | xclip -selection clipboard'

alias pretty-json='python -m json.tool'

#alias du-here='du -h -d 1 | sort -hr | head --lines 20'
alias du-here="du --human-readable --max-depth=1 | sort --human-numeric-sort --reverse | head --lines 20"

alias tar-to='tar -xzvf '

alias last-installed='cat /var/log/pacman.log | grep "installed" | tail -n 10'

alias update-grub='grub-mkconfig -o /boot/grub/grub.cfg'

alias emu='android-emulator'

# --- Starship Prompt ----------------------------------------------------------

eval "$(starship init zsh)"

# --- ENV ----------------------------------------------------------------------

# export SHELL=/usr/bin/zsh
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim

export DOTNET_ROOT=$HOME/software/dotnet7.0.9
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export DOTNET_CLI_TELEMETRY_OPTOUT=1 # 1 to refuse

export RUST_BIN=$HOME/.cargo/bin

# For endeavourOS
export LOCAL_BIN=$HOME/.local/bin

export GHC_HOME=$HOME/.ghcup/ghc/9.2.8/bin
export GHC_TOOLS_HOME=$HOME/.ghcup/bin

export ANDROID_HOME=$HOME/Android/Sdk

# Raylib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/lib/pkgconfig:$HOME/.pkgconfig

export PATH=$PATH:$RUST_BIN:$LOCAL_BIN:$DOTNET_ROOT:\
$DOTNET_TOOLS_HOME:$GHC_HOME:$GHC_TOOLS_HOME:$ANDROID_HOME


# --- Keybinds -----------------------------------------------------------------

# \e, \E, = Escape
# ^[      = Alt key (on some keyboards this is the same as escape)
# ^?      = Delete
# ^X, ^   = Control

#bindkey -e # Emacs Mode

bindkey -v # Enables vi mode

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
bindkey '^[Oc'    forward-word       #
bindkey '^[Od'    backward-word      #
bindkey '^[[1;5D' backward-word      #
bindkey '^[[1;5C' forward-word       #
bindkey '^H'      backward-kill-word # delete previous word with ctrl+backspace
bindkey '^[[Z'    undo               # Shift+tab undo last action

# Removing keys
bindkey -r '^J' # accept-line
bindkey -r '^K' # kill-line

# Up/Down line search (Must be after Vi mode - Vi mode bugs ^N and ^P)
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward

bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# --- Completion ---------------------------------------------------------------

zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' # Case insensitive tab completion
#zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"         # Colored completion (different colors for dirs/files/etc)
#zstyle ':completion:*' rehash true                              # automatically find new executables in path

# --- History ------------------------------------------------------------------

HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=500

# --- Options ------------------------------------------------------------------

setopt nobeep # No beep
