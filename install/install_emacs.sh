#!/bin/bash

sudo apt install -y libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgtk2.0-dev imagemagick
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa
sudo apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev
mkdir -p /tmp/install

cd /tmp/install

git clone git://git.sv.gnu.org/emacs.git

cd /tmp/install/emacs

git fetch origin feature/pgtk:feature/pgtk
git checkout feature/pgtk

export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10


./autogen.sh
./configure --with-cairo \
            --with-modules \
            --without-compress-install \
            --with-x-toolkit=no \
            --with-gnutls \
            --without-gconf \
            --without-xwidgets \
            --without-toolkit-scroll-bars \
            --without-xaw3d \
            --without-gsettings \
            --with-mailutils \
            --with-native-compilation \
            --with-json \
            --with-svg \
            --with-harfbuzz \
            --with-imagemagick \
            --with-jpeg \
            --with-png \
            --with-rsvg \
            --with-tiff \
            --with-wide-int \
            --with-xft \
            --with-xml2 \
            --with-xpm \
            CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j8 NATIVE_FULL_AOT=1
sudo checkinstall
