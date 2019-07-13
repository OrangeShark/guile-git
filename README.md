# Guile-Git

Guile-Git is a GNU Guile library providing bindings to
[libgit2](https://libgit2.org/).

Copyright © 2016, 2017 Amirouche Boubekki  
Copyright © 2018, 2019 Erik Edrosa  
Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.  This file is offered as-is,
without any warranty.

## Requirements

Guile-Git requires the following to be installed.

- [GNU Guile](https://www.gnu.org/software/guile/) >= 2.0.11
- [libgit2](https://libgit2.org/)
- [scheme-bytestructures](https://github.com/TaylanUB/scheme-bytestructures)

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

A common issue you may encounter when installing, especially on Ubuntu
and Fedora, is guild not being found when you run `configure`. This
happens because the configure script finds a symbolic link to a
version of guile with a version number, like guile-2.2. This then has
the configure script check for guild-2.2 which does not exist. To fix
this, you can provide the path to the version of guile you want to
install it for.

```sh
./configure GUILE=$(which guile)
```

## Contributing

The easiest way to start hacking on guile-git is to install
[GNU Guix](https://gnu.org/s/guix) and run the following command:

```bash
> guix environment -l guix.scm
```

To build from git you do:

```sh
./bootstrap
./configure
make
```

To run the unit tests you do:

```sh
make check
```

You can then:

- Create a pull request on gitlab
- Send a patch to one of the maintainers
- Come and ping `OrangeShark` about it at `#guile@irc.freenode.net`.

And don't forget to add a unit test!

## License

Guile-Git is licenced under GPLv3 or later. See COPYING file for
details.