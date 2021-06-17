#!/bin/zsh
# Dustin's Apple macOS setup
# Initializes scripts to handle errors gracefully

# Bash script initialization

# Exit as soon as any nonzero code is found
# Examples: undefined variables, failed pipes, etc.
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
