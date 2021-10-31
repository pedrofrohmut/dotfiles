if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES
alias vim='nvim'
alias gits='git status'
alias ll='ls -lA'
alias hello='echo Hello World!'

# --- Starship Prompt
starship init fish | source

# --- Remove the greetings message
set -U fish_greeting
