if status is-interactive
    # Commands to run in interactive sessions can go here
end

# --- ALIASES
alias vim='nvim'
alias gvim='nvim-qt'
alias gits='git status'
alias ll='ls -lA'

# --- Starship Prompt
starship init fish | source
