# Zsh Prompt
#PS1="%{$fg[green]%}%n%{$fg_bold[red]%}%~%{$reset_color%}»"

if [[ "$EUID" -ne "0" ]]
then  # if user is not root
	USER_LEVEL="%{$fg_bold[green]%}"
else # root!
	USER_LEVEL="%{$fg_bold[red]%}"
fi

PS1="%{$USER_LEVEL%}%~ %{$fg_bold[red]%}» %{$reset_color%}"
