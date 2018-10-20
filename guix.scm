

(use-modules ((guix licenses) #:select (gpl3+))
             (guix packages)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages compression)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tls)
             (gnu packages version-control))

(package
  (name "guile-git")
  (version "0.1")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs
   `(("guile" ,guile-2.2)
     ("libgit2" ,libgit2)
     ("openssl" ,openssl)
     ("zlib" ,zlib)
     ("guile-bytestructures" ,guile-bytestructures)))
  (synopsis "Guile bindings for libgit2")
  (description "")
  (home-page "")
  (license gpl3+))
