# DEPENDENCIES
# xclip, nvim, git, tree, starship

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES
alias vim="nvim"

alias gits='git status'
alias gitl='git log -n 5'

alias ll='ls -lA'
alias cll='clear; ls -lA'

alias tree='tree -C'

alias jj='jobs'

alias cpath='pwd | xclip -selection clipboard'

# --- Starship Prompt
starship init fish | source

# --- Remove the greetings message
set -U fish_greeting
