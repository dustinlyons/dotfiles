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
export FULL_NAME="Dustin Lyons"
export EMAIL="hello@dustinlyons.co"
export APPLE_ID=me@dustinlyons.co
export HOME_DIR=/Users/dustin
export DRAFTS_APP_ID=1435957248
export HARVEST_APP_ID=506189836
export HIDDEN_BAR_APP_ID=1452453066
export ONEPASSWORD_APP_ID=1333542190
export INSTAPAPER_APP_ID=288545208

##############################
# Error handling, interrupts 
##############################
set -o errexit
set -o nounset
set -o pipefail

# Progress spinner
spinner() {
    local i sp n
    sp='/-\|'
    n=${#sp}
    printf ' '
    while sleep 0.1; do
        printf "%s\b" "${sp:i++%n:1}"
    done
}
# Zsh error function
function handle_error() {
	echo -e "\e[1;31mOops! Something went wrong.\e[0m"
        exit 1
}

function handle_exit() {
        exit 0
}

# Always grab these signals and route them to my exit func
trap handle_error SIGHUP SIGINT SIGQUIT SIGABRT SIGTERM
trap handle_exit 0 EXIT

echo -e "\e[1;32mStarting private setup...\e[0m"
##############################
# Install homebrew
##############################

# Check for Homebrew,install if we don't have it
if [ ! -e "$(which brew)" ]; then
  echo -e "\e[1;32mInstalling homebrew...\e[0m"
  /bin/bash -c \
  "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> $HOME_DIR/.zprofile
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
if [ ! -e "$(which brew)" ]; then
  echo -e "\e[1;31mFailed to install homebrew, exiting...\e[0m"
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
echo -e "\e[1;32mUpdating homebrew...\e[0m"
brew update

echo -e "\e[1;32mInstalling git...\e[0m"
brew install git

echo -e "\e[1;32mConfiguring git...\e[0m"

git config --global user.name $FULL_NAME
git config --global user.email $EMAIL

echo -e "\e[1;32mInstalling homebrew packages...\e[0m"
brew install ${PACKAGES[@]}

##############################
# Install apps
##############################

# Apps
APPS=(
	appcleaner
	brave-browser
	discord
	docker
	google-chrome
	iterm2
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
echo -e "\e[1;32mInstalling homebrew apps...\e[0m"
brew install --force --appdir="/Applications" ${APPS[@]}
brew cleanup

##############################
# Mac app store
##############################
# mas signin $APPLE_ID
# Error: The 'signin' command has been disabled on this macOS version.
# For more info see: https://github.com/mas-cli/mas/issues/164
echo -e "\e[1;33mPlese open the Mac App Store and login\e[0m"
echo -e "\e[1;33mPress [Enter] key when ready...\e[0m"
echo -e "\e[1;33m...\e[0m"
read

echo -e "\e[1;32mInstalling Mac App Store apps...\e[0m"
mas install $DRAFTS_APP_ID
mas install $HARVEST_APP_ID
mas install $HIDDEN_BAR_APP_ID
mas install $ONEPASSWORD_APP_ID
mas install $INSTAPAPER_APP_ID

##############################
# Defaults prefs
##############################

echo -e "\e[1;32mSetting security defaults...\e[0m"
# Turn off the "Are you sure you want to open this?" warnings
defaults write com.apple.LaunchServices LSQuarantine -bool NO

echo -e "\e[1;32mComplete, rebooting finder...\e[0m"
# Enable settings
killall Finder
