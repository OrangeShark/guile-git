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

## How to contribute

The easiest way to start hacking on guile-git is to install
[GNU Guix](https://gnu.org/s/guix) and run the following command:

```bash
> guix environment -l guix.scm
```

You can then:

- Create a pull request on gitlab
- Send a patch to one of the maintainers
- Come and ping `amz3` or `OrangeShark` about it at `#guile@irc.freenode.net`.

And don't forget to add a unit test!
