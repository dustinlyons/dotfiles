#!/bin/zsh
# Dustin's macOS setup scripts
# 
# This is the "public" side of my macOS setup that
# handles key creation, git setup, and downloading of
# my private dotfiles to continue setup.
#
# I intend to upgrade Macs over the next several years. 
# These scripts will ensure I do not lose important 
# workflow optimizations.

##############################
# Error handling, interrupts 
##############################

source ./init.sh
echo "Starting public setup..."

##############################
# Install Xcode CLI tools
##############################

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

##############################
# Create new SSH key
##############################

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

##############################
# Download my dotfiles
##############################

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

##############################
# Source the rest 
##############################

[ -f "macos/macos-setup-private.sh" ] && source macos/macos-setup-private.sh
