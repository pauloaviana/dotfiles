#!/bin/sh

fzf --layout reverse --info inline --border -m -q '.pdf | .djvu '| sed 's/^/\"/; s/$/\"/;' | xargs -r zathura --fork
