# --- Aliases/Functions --------------------------------------------------------

alias src-rc='source ~/.zshrc'

alias vim='nvim'

alias hx='helix'

alias gits='git status'
alias gitp='git push'
alias gitac='git add . && git commit -m'

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
alias cll='clear; ls -lAF'
alias tree='tree -C'

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

# --- Starship Prompt ----------------------------------------------------------
eval "$(starship init bash)"
