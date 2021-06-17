#!/bin/zsh
# Dustin's macOS setup scripts
# 
# This is the "private" side of my macOS setup that
# handles dev configuration, app installation, and
# any Mac preferences.
#
# I intend to upgrade Macs over the next several years. 
# These scripts will ensure I do not lose important 
# workflow optimizations.

##############################
# Variables
##############################
export HOME_DIR=/Users/dustin

##############################
# Error handling, interrupts 
##############################
set -o errexit
set -o nounset
set -o pipefail

# Bash error function
function handle_error() {
        echo "Oops! Something went wrong."
        exit 1
}

function handle_exit() {
        exit 0
}

# Always grab these signals and route them to my exit func
trap handle_error SIGHUP SIGINT SIGQUIT SIGABRT SIGTERM
trap handle_exit 0 EXIT

echo "Starting private setup..."
##############################
# Install homebrew
##############################

# Check for Homebrew,
# Install if we don't have it
if [ ! -e "$(which brew)" ]; then
  echo "Installing homebrew..."
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> $HOME_DIR/.zprofile
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ -e "$(which brew)" ]; then
  echo "... installed."
else
  echo "Failed to install homebrew, exiting..."
  exit 1
fi

PACKAGES=(
	ctags
	htop
	jq
	fd
	flac
	mas
	ngrep
	node
	neovim
	watch
)

# Update homebrew recipes
echo "Updating homebrew..."
brew update

echo "Installing Git..."
brew install git

echo "Configuring git..."

git config --global user.name "Dustin Lyons"
git config --global user.email hello@dustinlyons.co

brew install ${PACKAGES[@]}

##############################
# Install apps
##############################

# Apps
APPS=(
	1password
	appcleaner
	brave-browser
	discord
	docker
	google-chrome
	iterm2
	hiddenbar
	notion
	ngrok
	kap
	raycast
	rocket
	spotify
	telegram
	homebrew/cask/transmission
	vlc
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "Installing apps..."
brew install --force --appdir="/Applications" ${APPS[@]}

brew cleanup

##############################
# Misc settings
##############################
