#!/usr/bin/bash

source export_vars.sh

if ! command_exists wine ; then
  $pacman -S wine \
             winetricks \
             wine-mono \
             wine-gecko \
             wine-nine
fi

WINEARCH=win32 winetricks win7 sound=pulse psm=3 mwo=enabled multisampling=enabled mimeassoc=off macdriver=x11 isolate_home hosts heapcheck gsm=3 glsl=enabled fontsmooth=rgb fontfix ddr=opengl cfc=enabled

fonts=(corefonts tahoma georgia droid courier consolas arial)

for font in "${fonts[@]}"; do
  echo "Apply font: $font"
  winetricks $font
  echo "Finish -------------------"
done

dlls=(d3dcompiler_47 d3dx10 d3dx11_43 d3dx9 d9vk dinput8 dmusic dotnet40 dxvk galliumnine msxml6 python27 vb6run ffdshow xvid)

for dll in "${dlls[@]}"; do
  echo "Apply dll: $dll"
  winetricks $dll
  echo "Finish -------------------"
done
