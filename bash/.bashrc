#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export EDITOR="/usr/bin/vim"

# Pretty colors
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Keybindings
xmodmap ~/.Xmodmap

# Start a command in the background with no stderr/stdout
start(){ command $@ &> /dev/null & }

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
