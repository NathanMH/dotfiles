#!/bin/bash
#WALLDIR=~/Documents/wallpapers/*
WALLDIR=/mnt/z/PortableApps/Wallpapers/*.jpg
for f in $WALLDIR
do
    echo $f
    wal --backend wal -q -n -i $f
done
