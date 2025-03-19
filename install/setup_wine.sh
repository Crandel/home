#!/usr/bin/env bash

winetricks win7 \
           sound=pulse \
           psm=3 \
           mwo=enabled \
           mimeassoc=off \
           macdriver=x11 \
           isolate_home \
           hosts \
           heapcheck \
           gsm=3 \
           shader_backend=glsl \
           fontsmooth=rgb \
           fontfix \
           renderer=gl \
           cfc=enabled

for font in "${fonts[@]}"; do
  echo "Apply font: $font"
  winetricks $font
  echo "Finish -------------------"
done

dlls=(d3dcompiler_47 d3dx10 d3dx11_43 d3dx9 dinput8 dmusic dotnet40 dxvk galliumnine msxml6 python27 vcrun2008 vb6run)

for dll in "${dlls[@]}"; do
  echo "Apply dll: $dll"
  winetricks $dll
  echo "Finish -------------------"
done
