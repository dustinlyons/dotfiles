# Dustin's .zshrc configuration

export PATH=$HOME/bin:$HOME/.local/bin:$PATH
export ZSH="/home/dustin/.oh-my-zsh"
export EDITOR="/usr/bin/nvim"
export TODOTXT_DEFAULT_ACTION="ls"
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export PIPENV_VERBOSITY=-1
# Don't dirty up the place with __pycache__ files
export PYTHONDONTWRITEBYTECODE=True
# Tells less not to paginate if less than a page
export LESS="-F -X $LESS" 

alias vim=nvim # visual vim, should I switch to neo?
alias vi=nvim # visual vim, should I switch to neo?
alias open='thunar . > /dev/null 2>&1' # open thunar where we are
alias t=todo.sh
alias w='todo.sh -d ~/.todo/work_config'
alias tmux=tmux -f /home/dustin/.tmux/tmux.conf
alias ss=switchsink
alias pylint=pylint --load-plugins pylint_flask_sqlalchemy
alias act='source venv/bin/activate'
alias up='docker-compose up --force-recreate --detach'
alias up='docker-compose up --force-recreate --detach'
alias down='docker-compose down'

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Add wisely, as too many plugins slow down shell startup.
plugins=(git)
source $ZSH/oh-my-zsh.sh

# Set keyboard repeat rate
xset r rate 200 40
