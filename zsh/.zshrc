# Dustin's .zshrc configuration

# Path stuff
export PATH=$HOME/bin:$HOME/.local/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/lib
export ZSH="/home/dustin/.oh-my-zsh"

# Oh-my-zsh init
export ZSH_THEME="robbyrussell" # See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
export MODE_INDICATOR="%F{yellow}+%f"
export VI_MODE_RESET_PROMPT_ON_MODE_CHANGE=true

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

# Take screenshots easily
alias shot=scrot -s '%Y-%m-%d_$wx$h.png' --exec 'mv $f ~/images/screenshots/'

# Load terminal
plugins=(
    git
    colored-man-pages
    vi-mode
    command-not-found
    web-search
)
source $ZSH/oh-my-zsh.sh

# Set keyboard repeat rate
xset r rate 200 40
