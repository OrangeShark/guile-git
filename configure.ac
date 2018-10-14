
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
