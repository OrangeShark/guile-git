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
  git/config.scm                    \
  git/oid.scm                       \
  git/object.scm                    \
  git/reference.scm                 \
  git/repository.scm                \
  git/reset.scm	                    \
  git/remote.scm                    \
  git/status.scm                    \
  git/structs.scm                   \
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

TESTS_UTILS =                                   \
  tests/helpers.scm                             \
  tests/data/simple-bare.tgz                    \
  tests/data/simple.tgz

TESTS =                                         \
  tests/branch.scm                              \
  tests/commit.scm                              \
  tests/oid.scm                                 \
  tests/reference.scm                           \
  tests/repository.scm                          \
  tests/reset.scm                               \
  tests/remote.scm                              \
  tests/status.scm                              \
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
  guix.scm                          \
  build-aux/test-driver.scm		  		\
  static/bg.png                     \
  static/main.css                   \
  static/normalize.css              \
  $(TESTS)                          \
  $(TESTS_UTILS)
