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
unsetopt beep

source ~/.zsh/aliases.zsh
source ~/.zsh/exports.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/zle.zsh

