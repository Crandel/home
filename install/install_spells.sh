#!/usr/bin/bash

source export_vars.sh

$pacman -S aspell-de \
           aspell-en \
           aspell-ru \
           aspell-uk \
           hunspell-de \
           hunspell-en_US

$yay -S hunspell-ru \
        hunspell-uk
