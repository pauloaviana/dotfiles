#!/bin/sh

source colors.sh

fzf --layout reverse --info inline --border --color fg:$color1,fg+:$color2,bg:$color0,border:$color3 -m -q '.pdf | .djvu '| sed 's/^/\"/; s/$/\"/;' | xargs -r zathura --fork
