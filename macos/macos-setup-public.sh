#!/bin/zsh
# Dustin's Apple macOS setup
# Public script to bootstrap before my git repos

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Bash script initialization
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Exit as soon as any nonzero code is found
# Examples: undefined variables, failed pipes, etc.
set -o errexit
set -o nounset
set -o pipefail

# Bash error function
function handle_error() {
	echo "Oops! Something went wrong. Cleaning up..."
	rm -rf ~/tmp
	exit 1
}

function handle_exit() {
	exit 0
}

# Always grab these signals and route them to my exit func
trap handle_error SIGHUP SIGINT SIGQUIT SIGABRT SIGTERM
trap handle_exit 0 EXIT

echo "Starting setup..."

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Install Xcode CLI tools
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if [ ! -d "/Library/Developer/CommandLineTools" ] 
then
	xcode-select --install
	echo "Waiting on Xcode tools to install..."
	echo "Press [Enter] key after installation."
	read
fi
if [ ! -d "/Library/Developer/CommandLineTools" ] 
then
	echo "Xcode tools not found, try again."
	exit 1
fi

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Create a new SSH key
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if [ ! -d ".ssh" ]
then
	echo "Creating SSH key..." && \
	ssh-keygen -t rsa
	echo "Please add this public key to Github: "

	cat ~/.ssh/id_rsa.pub

	echo "Link: https://github.com/account/ssh \n"
	echo "Press [Enter] key after this..."
	read
else
	echo "Found an SSH key, moving on..."

fi

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Download my dotfiles
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if [ ! -d "tmp" ] 
then
	echo "Creating tmp directory..." && \
	mkdir tmp
else
	echo "tmp directory already exists"
fi

cd ./tmp

if [ ! -d "dotfiles" ] 
then
	echo "Cloning dotfiles..."
	git clone git@github.com:dustinlyons/dotfiles.git && \
	cd dotfiles
else
	echo "dotfiles directory already exists, let's update to the latest..."
	cd dotfiles
	git pull origin master
fi

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Source the rest 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

[ -f "macos/macos-setup-private.sh" ] && source macos/macos-setup-private.sh
