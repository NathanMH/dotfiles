#!/bin/sh

cp -f ~/.cache/wal/colors.json /tmp/fzwal-backup.json

#THEME=$(ls ~/Documents/wallpapers | fzf --preview='wal -i ~/Documents/wallpapers/{} && wal --preview')
THEME=$(ls ~/.cache/wal/schemes/ | fzf --preview='wal --theme ~/.cache/wal/schemes/{} && wal --preview')

if [ -n "$THEME" ]; then
    wal -q --theme $THEME
else
    wal -q --theme /tmp/fzwal-backup.json
fi
