include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

SOURCES =                           \
  git/config.scm                    \
  git/types.scm                     \
  git/bindings.scm                  \
  git/reference.scm                 \
  git/repository.scm                \
  git/annotated.scm                 \
  git/attr.scm                      \
  git/blame.scm                     \
  git/tree.scm

TESTS =                             \
  tests/repository.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST +=                       \
  README.md                         \
  pre-inst-env.in                   \
  guix.scm
