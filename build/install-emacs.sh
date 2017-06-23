#!/bin/sh

version=25.2
patchname=25.2-mac-6.4

# where this script is
cwd=`dirname "${0}"`
expr "${0}" : "/.*" > /dev/null || cwd=`(cd "${cwd}" && pwd)`
cd ${cwd}

curl -O http://ftp.gnu.org/gnu/emacs/emacs-$version.tar.gz
curl -O ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-$patchname.tar.gz
curl -O ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-hires-icons-2.0.tar.gz
tar zxf emacs-$version.tar.gz
tar zxf emacs-$patchname.tar.gz
tar zxf emacs-hires-icons-2.0.tar.gz

cd emacs-$version
patch -p1 < ../emacs-$patchname/patch-mac
cp -R ../emacs-$patchname/mac .
cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
cp ../emacs-hires-icons-2.0/etc/images/* ./etc/images/
cp ../emacs-$patchname/src/* ./src/
cp ../emacs-$patchname/lisp/term/mac-win.el ./lisp/term/
CC="clang -fobjc-arc"

cd ../
./build-emacs.app.sh emacs-$version
