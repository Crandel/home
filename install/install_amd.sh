#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "amd-ucode"
  "amdvlk"
  "hip-runtime-amd"
  "lib32-amdvlk"
  "lib32-vulkan-radeon"
  "libva-mesa-driver"
  "mesa-vdpau"
  "nvtop"
  "radeontop"
  "rocm-core"
  "rocm-device-libs"
  "rocm-hip-runtime"
  "rocm-opencl-runtime"
  "vulkan-radeon"
  "vulkan-tools"
)

install_pacman $packages
