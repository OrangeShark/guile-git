dnl Guile-Git --- GNU Guile bindings of libgit2
dnl Copyright © 2016-2018 Erik Edrosa <erik.edrosa@gmail.com>
dnl Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
dnl
dnl This file is part of Guile-Git.
dnl
dnl Guile-Git is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl Guile-Git is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([Guile-Git], [0.1.1], [], [], [https://gitlab.com/guile-git/guile-git/])
AC_CONFIG_SRCDIR(git)
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign color-tests])

dnl Enable silent rules by default.
AM_SILENT_RULES([yes])

GUILE_PKG([2.2 2.0])
GUILE_PROGS

dnl (srfi srfi-64) appeared in Guile 2.0.11.
GUILE_MODULE_REQUIRED([srfi srfi-64])

GUILE_MODULE_REQUIRED([bytestructures guile])

PKG_CHECK_MODULES([LIBGIT2], [libgit2])
PKG_CHECK_VAR([LIBGIT2_LIBDIR], [libgit2], [libdir])
AC_MSG_CHECKING([libgit2 library path])
AS_IF([test "x$LIBGIT2_LIBDIR" = "x"], [
  AC_MSG_FAILURE([Unable to identify libgit2 lib path.])
])
AC_SUBST([LIBGIT2_LIBDIR])

AC_CONFIG_FILES([Makefile git/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])

AC_OUTPUT
