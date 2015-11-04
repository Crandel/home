#!/usr/bin/env fish
# -*-  mode:fish; tab-width:4  -*-

function install_vim_bundles
    set -l ROOT $HOME/.vim/bundle
    cd $ROOT/YouCompleteMe
    python2 install.py --gocode-completer
    cd ~
end
