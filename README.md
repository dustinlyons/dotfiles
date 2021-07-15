# dotfiles

Bootstrap and configuration I use across my Arch and MacOS environments.

## Features
![Screenshot showing desktop](images/FelixDesktop.png)

## Usage

### Stow
The actual dotfiles are handled by [Stow](http://www.gnu.org/software/stow/)
and as such live in the `stowable` directory. Each "package" should have its
own folder containing the various configs.

By default, configs will be linked within `~/.config/<package>`. That can be
changed by dropping a `.stowtarget` file in the package with the desired path.

### Systemd

## Description
Here is a collection of my personal dotfiles, which include:

- Firefox custom theme
- Openbox configuration and keybindings
- Polybar configuration and scripts
- A custom .Xresources color palette
- Rofi custom menus and configurations
- Alacritty configuration and color scheme
- Neovim configuration and color scheme
- Zsh configs
- Dunst notifications
- Picom configuration