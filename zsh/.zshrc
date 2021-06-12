# Dustin's .zshrc configuration

# Path stuff
export PATH=$HOME/bin:$HOME/.local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib
export ZSH="/home/dustin/.oh-my-zsh"
ZSH_THEME="robbyrussell" # See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes

# Opens dir in UI
alias open='thunar . > /dev/null 2>&1' # open thunar where we are

# Tells less not to paginate if less than a page
export LESS="-eirMX"

# Run tmux with my config, duh
alias tmux=tmux -f /home/dustin/.tmux/tmux.conf

# Moves between speakers and headphones when Arch isn't complaining
alias ss=switchsink

# Restarts polybar when Arch is in a bad mood
alias poly='killall -9 polybar; polybar -r bar &!' # if polybar misbehaves

# Docker
alias up='docker-compose up --force-recreate --detach'
alias down='docker-compose down'

# Python
alias act='source venv/bin/activate'
alias pylint=pylint --load-plugins pylint_flask_sqlalchemy
export PIPENV_VERBOSITY=-1
export PYTHONDONTWRITEBYTECODE=True

# Vim/neovim
alias vim=nvim 
alias vi=nvim
export EDITOR="/usr/bin/nvim"
export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# Play WoW in linux
alias wow='lutris rungame/battlenet &!'

# Load terminal
plugins=(git)
source $ZSH/oh-my-zsh.sh

# Set keyboard repeat rate
xset r rate 200 40
