## Prereqs
+ Git 2.23+
+ Emacs 26.1+ (*27 is recommended*) with GNUTLS support
+ [ripgrep] 11.0+
+ GNU `find`
+ *OPTIONAL:* [fd] 7.3.0+ (improves file indexing performance for some commands)

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
