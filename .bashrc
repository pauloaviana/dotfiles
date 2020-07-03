#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias r="ranger"
alias cc="calcurse"
alias nb="newsboat"
alias v="vim"
alias yacy="startYACY.sh"

# OLD_PS1='[\u@\h \W]\$ '
PS1="\[\e[33m\][\[\e[m\]\[\e[35m\]\u\[\e[m\]\[\e[33m\]@\[\e[m\]\[\e[34m\]\h\[\e[m\] \W\[\e[33m\]]\[\e[m\]\[\e[33m\]\\$\[\e[m\] "

export PATH="$HOME/Make/yacy:$HOME/.gem/ruby/2.7.0/bin:$HOME/.cargo/bin:$HOME/.config/scripts:$PATH"
export TERMINAL="st"
export EDITOR="vim"


#pass
export PASSWORD_STORE_CLIP_TIME=10
#Vi mode
set -o vi
