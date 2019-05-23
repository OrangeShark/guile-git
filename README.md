# Guile-Git

Guile-Git is a GNU Guile library providing bindings to
[libgit2](https://libgit2.org/).

Copyright © 2016, 2017 Amirouche Boubekki
Copyright © 2018 Erik Edrosa
Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.  This file is offered as-is,
without any warranty.

## Documentation

The documentation is currently a work in progress, so please read the
source and have a look at the [libgit2 API](https://libgit2.org/libgit2/#HEAD).

## Installation

Guile-Git uses GNU autotools to create the build and installation
scripts. The default install location is in `/usr/local`. From a
release tarball you can run:

```sh
./configure
make
sudo make install
```

This will install Guile-Git with the prefix `/usr/local/`. This
might not be the default load path for your GNU Guile. You may choose
to change the prefix to your GNU Guile's location with `./configure
--prefix=/usr` or add `/usr/local/` to GNU Guile's load path in your
`.profile` or `.bash_profile` like this (replacing 2.2 with your GNU
Guile major version):

```sh
export GUILE_LOAD_PATH="/usr/local/share/guile/site/2.2${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/2.2/site-ccache${GUILE_LOAD_COMPILED_PATH:+:}$GUILE_COMPILED_LOAD_PATH"
```

## How to contribute

The easiest way to start hacking on guile-git is to install
[GNU Guix](https://gnu.org/s/guix) and run the following command:

```bash
> guix environment -l guix.scm
```

You can then:

- Create a pull request on gitlab
- Send a patch to one of the maintainers
- Come and ping `OrangeShark` about it at `#guile@irc.freenode.net`.

And don't forget to add a unit test!
