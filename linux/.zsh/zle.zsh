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

bindkey ' ' magic-space
bindkey -M vicmd "gg" beginning-of-history
bindkey -M vicmd "G" end-of-history
bindkey -M vicmd "/" history-incremental-search-backward
bindkey -M vicmd "?" history-incremental-search-forward
bindkey -M vicmd "u" undo
bindkey -M vicmd "_" beginning-of-line
bindkey -M vicmd "g_" end-of-line
bindkey -M vicmd ":" undefined-key # annoying default bind

zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

bindkey -M viins 'nn' vi-cmd-mode
bindkey -M viins "^I" expand-or-complete-prefix
# bindkey -M viins "^L" clear-screen

# Command History
bindkey -M vicmd "k" up-line-or-history # history-search-backward
bindkey -M vicmd "j" down-line-or-history # history-search-forward

# Movement
bindkey -M vicmd "H" beginning-of-line
bindkey -M vicmd "L" end-of-line


