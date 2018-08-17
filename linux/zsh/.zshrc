setopt correctall

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

source ~/.zsh/aliases.zsh
source ~/.zsh/exports.zsh
source ~/.zsh/prompt.zsh
source ~/.zsh/colors.zsh
source ~/.zsh/zle.zsh

if [ -f ~/Documents/dotfiles/linux/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]
then

    source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
    source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    # OLD source ~/Documents/dotfiles/linux/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets cursor)

    ZSH_HIGHLIGHT_STYLES[default]='none'
    ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta,bold'
    ZSH_HIGHLIGHT_STYLES[command]='fg=magenta,bold'
    ZSH_HIGHLIGHT_STYLES[path]='fg=yellow'
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=red'
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=red'
fi
