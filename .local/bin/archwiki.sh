#!/bin/sh

cd /usr/share/doc/arch-wiki/html/en

fzf --layout reverse --info inline --border --color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,border:#778899' -m | xargs -r qutebrowser
