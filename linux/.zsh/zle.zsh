# Zsh Keymap - VIM

function zle-keymap-select {
  zle reset-prompt

  if [[ $KEYMAP = "vicmd" ]]; then
    echo -ne "\033]12;5\007"
  else
    echo -ne "\033]12;6\007"
  fi
}

function zle-line-finish {
  zle reset-prompt

  echo -ne "\033]12;6\007"
}

autoload -U edit-command-line
zle -N zle-keymap-select
zle -N zle-line-finish
zle -N edit-command-line

bindkey -v
bindkey -M vicmd v edit-command-line # ESC-v to edit in an external editor.

# General
bindkey -M viins 'nn' vi-cmd-mode
bindkey -M viins "^n" expand-or-complete-prefix
bindkey -M vicmd "u" undo

# Command History
bindkey -M vicmd "k" up-line-or-history # history-search-backward
bindkey -M vicmd "j" down-line-or-history # history-search-forward
bindkey -M viins "^L" clear-screen
bindkey -M vicmd "/" history-incremental-search-backward
bindkey -M vicmd "?" history-incremental-search-forward
bindkey -M viins "^h" forward-char

# Movement
bindkey -M vicmd "H" beginning-of-line
bindkey -M vicmd "L" end-of-line


