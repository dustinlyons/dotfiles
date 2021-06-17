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
# Error handling, interrupts 
##############################

source ./init.sh
echo "Starting private setup..."

##############################
# Install homebrew
##############################

# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

PACKAGSE=(
	htop
	jq
)
# Update homebrew recipes
echo "Updating homebrew..."
brew update

echo "Installing Git..."
brew install git

echo "Configuring git..."

git config --global user.name "Dustin Lyons"
git config --global user.email hello@dustinlyons.co

##############################
# Install apps
##############################

# Apps
APPS=(
	1password
	1password-cli
	appcleaner
	brave-browser
	discord
	docker
	iterm2
	hiddenbar
	notion
	ngrok
	kap
	raycast
	rocket
	spotify
	telegram
	transmission
	vlc
)

# Install apps to /Applications
# Default is: /Users/$user/Applications
echo "installing apps with Cask..."
brew cask install --appdir="/Applications" ${apps[@]}

brew cask alfred link

brew cask cleanup
brew cleanup
brew cleanup

##############################
# Misc settings
##############################
