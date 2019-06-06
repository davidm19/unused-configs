#!/bin/bash

# Curie font install
cd ~
git clone https://github.com/nerdypepper/curie
ln -fs ~/curie/regular/curieMedium-12.bdf ~/.local/share/fonts/curieMedium-12.bdf
ln -fs ~/curie/bold/curieBold-12.bdf ~/.local/share/fonts/curieBold-12.bdf
ln -fs ~/curie/italic/curieItalic-12.bdf ~/.local/share/fonts/curieItalic-12.bdf

# Terminus font install
mkdir -p ~/.local/share/fonts
URL1="https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Terminus/terminus-ttf-4.40.1/Regular/complete/Terminess%20(TTF)%20Nerd%20Font%20Complete.ttf"
URL2="https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Terminus/terminus-ttf-4.40.1/Regular/complete/Terminess%20(TTF)%20Nerd%20Font%20Complete%20Mono.ttf"
cd ~/.local/share/fonts && curl -fLo "Terminess (TTF) Nerd Font Complete.ttf" $URL1
curl -fLo "Terminess (TTF) Nerd Font Complete Mono.ttf" $URL2
cd ~
sudo fc-cache -fv
