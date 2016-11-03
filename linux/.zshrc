# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/musicnate/.zshrc'

autoload -Uz compinit colors && colors


HISTFILE=~/.bash_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
bindkey -v

source ~/Documents/dotfiles/linux/.zsh/aliases.zsh
source ~/Documents/dotfiles/linux/.zsh/exports.zsh
source ~/Documents/dotfiles/linux/.zsh/prompt.zsh
source ~/Documents/dotfiles/linux/.zsh/colors.zsh
source ~/Documents/dotfiles/linux/.zsh/zle.zsh
