#!/bin/sh

# Usage: 
# 1. clone source from mirror: git clone https://github.com/railwaycat/mirror-emacs-mac
# 2. have this script besides mirror directory
# 2. run script: `./build-emacs.app.sh  mirror-emacs-mac`
#   please notice that all files under target directory "./emacs-mac-build" will been cleared
# 3. That's all.

installprefix=`pwd`/emacs-mac-build
app_dir=$installprefix/Emacs.app/Contents/Resources
version=25.2
compver=x86_64-apple-darwin`uname -r`

if [ -d $installprefix ]; then
    echo "target directory $installprefix exists, exit"
    exit
fi

# make an emacs bundle
cd $1
find . -name *.elc | xargs rm
rm -rf $installprefix
mkdir $installprefix
set -e
./autogen.sh
./configure --with-mac --enable-mac-app=$installprefix --prefix=$installprefix
make
make install
set +e

mv $installprefix/share/emacs/$version/* $app_dir
mv $installprefix/share/info $app_dir
mv $installprefix/share/man $app_dir
rm -rf $installprefix/share
mv $installprefix/var $app_dir
mv $installprefix/bin $app_dir/../MacOS/bin
mv $installprefix/libexec/emacs/$version/$compver $app_dir/../MacOS/libexec
rm -rf $installprefix/libexec

echo 'Done! Find your Emacs.app at '$installprefix'.'
