dotfiles
========

Personal configuration files. You probably don't care.

## Installing Emacs

Currently on: Emacs 30.2

https://batsov.com/articles/2021/12/19/building-emacs-from-source-with-pgtk/

```
sudo apt install build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libncurses-dev texinfo autoconf
sudo apt install libjansson4 libjansson-dev
sudo apt install libgccjit0 libgccjit-10-dev gcc-10 g++-10 
sudo apt install libtree-sitter-dev

./autogen.sh
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10

./configure --with-tree-sitter --with-native-compilation --with-pgtk
make -j8
sudo make install
```

