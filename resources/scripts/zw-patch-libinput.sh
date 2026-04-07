#!/bin/bash

set -e

echo "Starting clean build for libinput..."
if [ -d "builddir/" ]; then
    echo "Removing existing builddir/..."
    rm -rf builddir/
fi

echo "Pull latest version..."
git reset --hard
git pull

echo "Applying patch..."
patch -Np1 -i ~/.emacs.d/resources/scripts/libinput-enbale-3fg-drag.patch

echo "Configuring with Meson..."
meson setup --prefix=/usr builddir/

echo "Compiling with Ninja..."
ninja -C builddir/

echo "Installing to system..."
sudo ninja -C builddir/ install

echo "Success! libinput with 3-finger drag has been installed."
