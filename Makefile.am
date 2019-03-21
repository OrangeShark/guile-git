# Guile-Git --- GNU Guile bindings of libgit2
# Copyright © 2016-2018 Erik Edrosa <erik.edrosa@gmail.com>
# Copyright © 2016, 2017 Amirouche Boubekki <amirouche@hypermove.net>
# Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
#
# This file is part of Guile-Git.
#
# Guile-Git is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Guile-Git is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =                           \
  git.scm                           \
  git/annotated.scm                 \
  git/attr.scm                      \
  git/bindings.scm                  \
  git/blame.scm                     \
  git/blob.scm                      \
  git/branch.scm                    \
  git/checkout.scm                  \
  git/cherrypick.scm                \
  git/clone.scm                     \
  git/commit.scm                    \
  git/cred.scm                      \
  git/errors.scm                    \
  git/fetch.scm                     \
  git/oid.scm                       \
  git/object.scm                    \
  git/reference.scm                 \
  git/repository.scm                \
  git/reset.scm	                    \
  git/remote.scm                    \
  git/rev-parse.scm                 \
  git/settings.scm                  \
  git/signature.scm                 \
  git/status.scm                    \
  git/structs.scm                   \
  git/submodule.scm									\
  git/tag.scm                       \
  git/tree.scm                      \
  git/types.scm                     \
  git/web.scm                       \
  git/web/html.scm                  \
  git/web/mime-types.scm            \
  git/web/querystring.scm           \
  git/web/http.scm                  \
  git/web/template.scm              \
  git/web/config.scm                \
  git/web/repository.scm

NODIST_SOURCES =                    \
  git/config.scm

TESTS_UTILS =                                   \
  tests/helpers.scm                             \
  tests/data/simple-bare.tgz                    \
  tests/data/simple.tgz                         \
  tests/data/README

TESTS =                                         \
  tests/branch.scm                              \
  tests/commit.scm                              \
  tests/oid.scm                                 \
  tests/reference.scm                           \
  tests/repository.scm                          \
  tests/reset.scm                               \
  tests/remote.scm                              \
  tests/rev-parse.scm                           \
  tests/status.scm                              \
  tests/submodule.scm														\
  tests/tag.scm                                 \
  tests/tree.scm

TEST_EXTENSIONS = .scm

SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

include $(top_srcdir)/doc/local.mk

EXTRA_DIST +=                       \
  README.md                         \
  pre-inst-env.in                   \
  build-aux/test-driver.scm		  		\
  static/bg.png                     \
  static/main.css                   \
  static/README                     \
  $(TESTS)                          \
  $(TESTS_UTILS)
