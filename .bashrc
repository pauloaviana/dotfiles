#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias v="vim"

PS1="\[\e[33m\][\[\e[m\]\[\e[35m\]\u\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[34m\]\h\[\e[m\] \W\[\e[33m\]]\[\e[m\]\[\e[33m\]\\$\[\e[m\] "

export TERMINAL="st"
export EDITOR="vim"

#Vi mode
set -o vi
