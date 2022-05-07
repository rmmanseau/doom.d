## Prereqs
+ Git 2.23+
+ Emacs 26.1+ (*27 is recommended*) with GNUTLS support
+ [ripgrep] 11.0+
+ GNU `find`
+ *OPTIONAL:* [fd] 7.3.0+ (improves file indexing performance for some commands)

## Install Emacs
```
git clone --depth=1 --single-branch --branch emacs-27 https://github.com/emacs-mirror/emacs.git

cd emacs/

sudo apt install -y autoconf make gcc texinfo libgtk-3-dev libxpm-dev \
     libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev \
     libjansson-dev libharfbuzz-dev libharfbuzz-bin

./autogen.sh

./configure --with-json --with-modules --with-harfbuzz --with-compress-install \
            --with-threads --with-included-regex --with-zlib --without-sound \
            --without-xpm --with-jpeg --without-tiff --without-gif --with-png \
            --without-rsvg --without-toolkit-scroll-bars \
            --without-gpm --without-dbus --without-makeinfo --without-pop \
            --without-mailutils --without-gsettings
make
sudo make install
```

## Install
``` 
$ git clone --single-branch --branch develop --depth 1 https://github.com/hlissner/doom-emacs .emacs.d 
$ git clone --depth 1 https://github.com/rmmanseau/doom.d .doom.d
$ .emacs.d/bin/doom install

# available toggles:
# cut:      use ~/cut/ as home instead of ~
# org:      org roam configuration
# arista:   arista configuration

# to enable:
$ touch .doom.d/toggle/<toggle>
```
