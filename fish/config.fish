if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES
alias vim='nvim -u ~/programming/nvimconfig/main.vim'
#alias v='nvim'
#alias evim='nvim -u ~/programming/nvimconfig-lua/init.lua'

alias gits='git status'
alias gitl='git log -n 5'

alias ll='ls -lA'
alias cll='clear; ls -lA'
alias tree='tree -C'
alias jj='jobs'

# --- Starship Prompt
starship init fish | source

# --- Remove the greetings message
set -U fish_greeting
