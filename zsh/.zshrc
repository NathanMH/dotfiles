setopt correctall

# zstyle ':completion:*' '' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/natha/.zshrc'

autoload -U compinit && compinit
autoload -Uz compinit colors && colors

HISTFILE=~/.bash_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -v
unsetopt beep listambiguous

source ~/.zsh/aliases.zsh
source ~/.zsh/exports.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/zle.zsh

export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0

#[ -f ~/.fzf.bash ] && source ~/.fzf.bash
source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
export FZF_DEFAULT_OPTS='--height 50% --layout=reverse --border --preview "bat --style=numbers --color=always {}"'
