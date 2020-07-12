# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/dustin/.oh-my-zsh"
export EDITOR="/usr/bin/nvim"
export TODOTXT_DEFAULT_ACTION="ls"

# My aliases
alias vim=nvim # visual vim, should I switch to neo?
alias vi=nvim # visual vim, should I switch to neo?
alias open='thunar . > /dev/null 2>&1' # open thunar where we are
alias cleanpkg=pacman -Rsn $(pacman -Qdtq) # remove unneeded dependencies
alias t=todo.sh
alias w='todo.sh -d ~/.todo/work_config'
alias notes='cd ~/docs/inbox ; vim'
alias tmux=tmux -f /home/dustin/.tmux/tmux.conf
alias ss=switchsink

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Add wisely, as too many plugins slow down shell startup.
plugins=(git)
source $ZSH/oh-my-zsh.sh

# Set keyboard repeat rate
xset r rate 200 40
