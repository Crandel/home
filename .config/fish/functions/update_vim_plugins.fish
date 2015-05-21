function update_vim_plugins
    cd ~/.vim/bundle
    pwd
    for i in (command ls);
        cd $i;
        git pull
        git submodule update --init --recursive
        cd ..
    end
end
