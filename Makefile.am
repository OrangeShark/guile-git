include guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

SOURCES =                           \
  git.scm                           \
  git/annotated.scm                 \
  git/attr.scm                      \
  git/bindings.scm                  \
  git/blame.scm                     \
  git/branch.scm                    \
  git/checkout.scm                  \
  git/commit.scm                    \
  git/config.scm                    \
  git/enums.scm                     \
  git/oid.scm                       \
  git/reference.scm                 \
  git/repository.scm                \
  git/structs.scm                   \
  git/tree.scm                      \
  git/types.scm                     \
  git/web.scm                       \
  git/web/html.scm                  \
  git/web/mime-types.scm            \
  git/web/querystring.scm           \
  git/web/http.scm                  \
  git/web/config.scm                \
  git/web/repository.scm

TESTS =                             \
  tests/branch.scm                  \
  tests/commit.scm                  \
  tests/reference.scm               \
  tests/repository.scm              \
  tests/tree.scm

TEST_EXTENSIONS = .scm

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST +=                       \
  README.md                         \
  pre-inst-env.in                   \
  guix.scm                          \
  static/bg.png                     \
  static/main.css                   \
  static/normalize.css
