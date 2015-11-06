#!/usr/bin/env fish
# -*-  mode:fish; tab-width:4  -*-

function install_vim_bundles
    set -l ROOT $HOME/.vim/bundle
    set -l ROOT_NVIM $HOME/.config/nvim/bundle
    cd $ROOT/YouCompleteMe
    python2 install.py --gocode-completer
    cd $ROOT_NVIM/YouCompleteMe
    python2 install.py --gocode-completer
    cd ~
end
