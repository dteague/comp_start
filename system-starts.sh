#!/bin/bash

#####################################
########## Apps Installing ##########
#####################################

# Firefox:      Internet
# Filezilla:    remote file mover
# Evince:       pdf viewer
# Audacious:    music player
# VLC:          video player
# Terminator:   Nice terminal emulator
# Open Office:  word, excel, powerpoint, etc
# Emacs:        godtier editor
# Kile:         LaTex editor
# Kate:         Nice out of terminal editor
# Konversation: IRC client
# Virtual Box:  Virtual machine
# gPodder:      Podcast stuff
# Tor:          Onion Router
# Gthumb:       Photo viewer
# Vim:          Other editor

sudo apt-get install filezilla && evince && audacious && vlc && terminator && libreoffice && emacs && kile && konversation && virtualbox && gpodder && firefox && gthumb && vim

sudo add-apt-repository ppa:webupd8team/tor-browser
sudo apt-get update
sudo apt-get install tor-browser

############################################
########## special visual install ##########
############################################

# Numix:        Window Manager Theme
# I3:           Window Manager, very minalistic
# Zukitke:      GTK theme, minimal and round
# Bash it:      better verison of bash
# Oh My Zsh:    better verison of bash, but not bash
# Also adds configs for i3 and aliases from bash

sudo add-apt-repository ppa:numix/ppa
sudo apt-get update && sudo apt-get install numix-gtk-theme

sudo apt-get update
sudo apt-get --allow-unauthenticated install su5r-keyring
sudo apt-get update
sudo apt-get install i3


git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
. ~/.bash_it/install.sh

sudo apt-get install gtk2-engines-murrine gtk2-engines-pixbuf fonts -droid

sudo apt-get install wget
git clone https://github.com/robbyrussell/oh-my-zsh ~/.zsh
sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"

# run "chsh -s <shell path>" to change shell type

sudo chsh -s /bin/bash

cp aliases ~/.aliases
string="if [ -f ~/.aliases ] then; \   . ~/.aliases\fi"
echo $string >> .bashrc
echo $string >> .zshrc
cp i3config ~/.i3/config

#########################################
######### programming installs ##########
#########################################

# Installs compilers and python, python libraries and Ipython

sudo apt-get install gcc
sudo apt-get install python
pip install numpy
pip install scipy
pip install matplotlib
sudo apt-get install ipython

