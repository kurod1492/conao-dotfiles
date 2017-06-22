#!/bin/sh

version=25.2
patchversion=6.4
# curl -O http://ftp.gnu.org/gnu/emacs/emacs-$version.tar.gz
# curl -O ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-$version-mac-$patchversion.tar.gz
# curl -O ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-hires-icons-2.0.tar.gz
tar zxf emacs-$version.tar.gz
tar zxf emacs-$version-mac-$patchversion.tar.gz
tar zxf emacs-hires-icons-2.0.tar.gz

cd emacs-$version
patch -p1 < ../emacs-$version-mac-$patchversion/patch-mac
cp -R ../emacs-$version-mac-$patchversion/mac .
cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
cp ../emacs-hires-icons-2.0/etc/images/* ./etc/images/
cp ../emacs-$version-mac-$patchversion/src/* ./src/
cp ../emacs-$version-mac-$patchversion/lisp/term/mac-win.el ./lisp/term/
CC="clang -fobjc-arc"

cd ../
./build-emacs.app.sh emacs-$version