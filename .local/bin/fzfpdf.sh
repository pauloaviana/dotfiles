#!/bin/sh

fzf --layout reverse --info inline --border --color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,border:#778899' -m -q '.pdf | .djvu '| sed 's/^/\"/; s/$/\"/;' | xargs -r zathura --fork
