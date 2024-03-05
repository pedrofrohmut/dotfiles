# --- Aliases/Functions --------------------------------------------------------

alias src-rc='source ~/.zshrc'

alias vim='nvim'
alias old='/usr/local/bin/vim'

alias hx='helix'

alias emc='emacsclient -c .'

alias gits='git status'
alias gitp='git push'
alias gitac='git add . && git commit -m'
alias git-ops='git reset --soft HEAD^'

# Git diff
gitd() {
    if [ $1 ]; then
        git diff "$1" | bat
    else
        git diff . | bat
    fi
}

# Git logging
gitl() {
    if [ $1 ]; then
        git log -n "$1" | bat
    else
        git log -n 5 | bat
    fi
}

alias ls='ls --color=auto'
alias ll='ls -lAFh'
alias la='ls -1 -AFh'
alias cll='clear; ls -lAF'

alias tree='tree -C'
alias gtree='tree --gitignore'

alias fcd='cd $(find . -type d -print | fzf)'

alias exa='exa --icons=always'
alias els='exa'
alias ell='exa -lAFh'
alias etree='exa --tree'

alias ..='cd ..'
alias cp='cp --verbose'

alias dnb='dotnet build'
alias dnr='dotnet run'
alias dnwr='dotnet watch run'
alias dnt='clear && dotnet test'

alias jj='jobs'

alias c-path='pwd | xclip -selection clipboard'

alias du-here='du -h -d 1 | sort -hr | head --lines 20'
alias dh='du-here'

# Tar easy of use
tar-to() {
    if [ ! $1 ]; then
        echo "Missing file input"
        echo "Usage: tar-to <input-path> <output-path>"
    elif [ ! $2 ]; then
        tar -xzvf $1
    else
        tar -xzvf $1 -C $2
    fi
}

alias last-installed='cat /var/log/pacman.log | grep "installed" | tail -n 10'
alias installed="cat /var/log/pacman.log | grep 'ALPM] installed'"

alias update-grub='grub-mkconfig -o /boot/grub/grub.cfg'

# Setup Ocaml env
alias opam-env='eval $(opam env)'

alias kitty-themes='kitty +kitten themes'

# --- ENV ----------------------------------------------------------------------

# export SHELL=/usr/bin/zsh
export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim

# Dotnet
export DOTNET_ROOT=$HOME/software/dotnet7.0.9
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export DOTNET_CLI_TELEMETRY_OPTOUT=1 # 1 to refuse

export DOOM_EMACS_PATH=$HOME/.config/emacs/bin

export LOCAL_BIN=$HOME/.local/bin # For endeavourOS

export PATH=$PATH:$LOCAL_BIN:$DOOM_EMACS_PATH

# --- Keybinds -----------------------------------------------------------------

# \e, \E, = Escape
# ^[      = Alt key (on some keyboards this is the same as escape)
# ^?      = Delete
# ^X, ^   = Control

#bindkey -e # Emacs Mode

bindkey -v # Enables vi mode

#bindkey '^[[H'  beginning-of-line                    # Home key (xterm)
#bindkey '^[[OH' beginning-of-line                    # Home key (smkx mode)
#bindkey '^[[1~' beginning-of-line                    # Home key (screen & tmux)
#bindkey '^[[7~' beginning-of-line                    # Home key (urxvt)
#bindkey '^[[F'  end-of-line                          # End key (xterm)
#bindkey '^[[OF' end-of-line                          # End key (smkx mode)
#bindkey '^[[4~' end-of-line                          # End key (screen & tmux)
#bindkey '^[[8~' end-of-line                          # End key (urxvt)
#bindkey '^[[2~' overwrite-mode                       # Insert key
#bindkey '^[[3~' delete-char                          # Delete key
#bindkey '^[[C'  forward-char                         # Right key
#bindkey '^[[D'  backward-char                        # Left key
##bindkey '^[[5~' history-beginning-search-backward   # Page up key
##bindkey '^[[6~' history-beginning-search-forward    # Page down key
#bindkey '^[[A'  up-line-or-history                   # Up key
#bindkey '^[[B'  down-line-or-history                 # Down key

# Navigate words with ctrl+arrow keys
#bindkey '^[Oc'    forward-word       #
#bindkey '^[Od'    backward-word      #
#bindkey '^[[1;5D' backward-word      #
#bindkey '^[[1;5C' forward-word       #
#bindkey '^H'      backward-kill-word # delete previous word with ctrl+backspace
#bindkey '^[[Z'    undo               # Shift+tab undo last action

# Removing keys
bindkey -r '^J' # accept-line
bindkey -r '^K' # kill-line
bindkey -r '^U' # VTerm

# Up/Down line search (Must be after Vi mode - Vi mode bugs ^N and ^P)
bindkey "^P" history-search-backward
bindkey "^N" history-search-forward

#bindkey "^A" beginning-of-line
#bindkey "^E" end-of-line

# --- Completion ---------------------------------------------------------------

# init completion
autoload -U compinit; compinit

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# Show completion menu if more than one option is available
zstyle ':completion:*' menu select

# Ignore completion duplicates
zstyle ':completion:*' unique

# Autocomplete from history
zstyle ':completion:*' history 1

# Colored completion (different colors for dirs/files/etc)
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# automatically find new executables in path
zstyle ':completion:*' rehash true

# --- Options ------------------------------------------------------------------

setopt nobeep # No beep

# --- Evals --------------------------------------------------------------------

# Starship Prompt
eval "$(starship init zsh)"

# Zoxide
#eval "$(zoxide init zsh)"

# Opam (OCaml package manager)
eval "$(opam env)"
