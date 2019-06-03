;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
;;;
;;; This file is part of Guile-Git.
;;;
;;; Guile-Git is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-Git is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

(define-module (git describe)
  #:use-module (system foreign)
  #:use-module (git errors)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (DESCRIBE-MAX-CANDIDATES
            DESCRIBE-STRATEGY-DEFAULT
            DESCRIBE-STRATEGY-TAGS
            DESCRIBE-STRATEGY-ALL
            DESCRIBE-STRATEGY
            DESCRIBE-PATTERN
            DESCRIBE-ONLY-FOLLOW-FIRST-PARENT?
            DESCRIBE-FALLBACK-TO-OID?
            make-describe-options
            describe-commit
            describe-workdir

            DESCRIBE-FORMAT-ABBREVIATED-SIZE
            DESCRIBE-FORMAT-ALWAYS-USE-LONG-FORMAT?
            DESCRIBE-FORMAT-DIRTY-SUFFIX
            make-describe-format-options
            describe-format))

;;; https://libgit2.org/libgit2/#HEAD/group/describe

(define DESCRIBE-OPTIONS-VERSION 1)

(define DESCRIBE-MAX-CANDIDATES 10)
(define DESCRIBE-STRATEGY-DEFAULT 0)
(define DESCRIBE-STRATEGY-TAGS 1)
(define DESCRIBE-STRATEGY-ALL 2)
(define DESCRIBE-STRATEGY 'default)
(define DESCRIBE-PATTERN "")
(define DESCRIBE-ONLY-FOLLOW-FIRST-PARENT? #f)
(define DESCRIBE-FALLBACK-TO-OID? #f)

(define (symbol->describe-strategy symbol)
  (case symbol
    ((default) DESCRIBE-STRATEGY-DEFAULT)
    ((tags) DESCRIBE-STRATEGY-TAGS)
    ((all) DESCRIBE-STRATEGY-ALL)
    (else (raise-git-error GIT_EINVALID))))

(define make-describe-options
  ;; Return a <describe-options> structure for use with DESCRIBE-COMMIT
  ;; or DESCRIBE-WORKDIR.  MAX-CANDIDATES specifies how many tags will
  ;; be considered when finding the descriptive name; use 0 to not
  ;; consider any tags at all.  STRATEGY can be either 'default to only
  ;; consider annotated tags, 'tags to consider all tags, or 'all to
  ;; also consider branch names.  If PATTERN is a non-empty string, only
  ;; refs matching the given glob(7) pattern are considered.
  ;; ONLY-FOLLOW-FIRST-PARENT? will ignore tags that are not direct
  ;; ancestors of the commit.  When FALLBACK-TO-OID? is true, the commit
  ;; ID will be returned if no matching tags were found.
  (let ((proc (libgit2->procedure* "git_describe_init_options"
                                   `(* ,unsigned-int))))
    (lambda* (#:key
              (max-candidates DESCRIBE-MAX-CANDIDATES)
              (strategy DESCRIBE-STRATEGY)
              (pattern DESCRIBE-PATTERN)
              (only-follow-first-parent? DESCRIBE-ONLY-FOLLOW-FIRST-PARENT?)
              (fallback-to-oid? DESCRIBE-FALLBACK-TO-OID?))
      (let ((describe-options (make-describe-options-bytestructure))
            (strategy (symbol->describe-strategy strategy)))
        (proc (describe-options->pointer describe-options) DESCRIBE-OPTIONS-VERSION)
        (set-describe-options-max-candidates-tag! describe-options max-candidates)
        (when (> strategy 0)
          (set-describe-options-strategy! describe-options strategy))
        (when (> (string-length pattern) 0)
          (set-describe-options-pattern! describe-options (string->pointer pattern)))
        (when only-follow-first-parent?
          (set-describe-options-only-follow-first-parent! describe-options 1))
        (when fallback-to-oid?
          (set-describe-options-show-commit-oid-as-fallback! describe-options 1))

        describe-options))))

(define %describe-result-free (dynamic-func "git_describe_result_free" libgit2))

(define (pointer->describe-result! pointer)
  (set-pointer-finalizer! pointer %describe-result-free)
  (pointer->describe-result pointer))

(define* describe-commit
  (let ((proc (libgit2->procedure* "git_describe_commit" '(* * *))))
    (lambda* (commit #:optional (options (make-describe-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (commit->pointer commit)
              (describe-options->pointer options))
        (pointer->describe-result! (dereference-pointer out))))))

(define* describe-workdir
  (let ((proc (libgit2->procedure* "git_describe_workdir" '(* * *))))
    (lambda* (repository #:optional (options (make-describe-options)))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (describe-options->pointer options))
        (pointer->describe-result! (dereference-pointer out))))))



(define DESCRIBE-FORMAT-OPTIONS-VERSION 1)

(define DESCRIBE-FORMAT-ABBREVIATED-SIZE 7)
(define DESCRIBE-FORMAT-ALWAYS-USE-LONG-FORMAT? #f)
(define DESCRIBE-FORMAT-DIRTY-SUFFIX "")

;; Return a <describe-format-options> structure for formatting the
;; output of DESCRIBE-FORMAT.  ABBREVIATED-SIZE specifies how many
;; characters of the commit ID to use.  When ALWAYS-USE-LONG-FORMAT? is
;; true, the returned string will be formatted with tag, number of
;; commits and abbreviated commit ID even when it exactly matches a tag.
;; DIRTY-SUFFIX is an optional string that will be appended to the output
;; when the worktree is considered 'dirty', i.e. modified.
(define make-describe-format-options
  (let ((proc (libgit2->procedure* "git_describe_init_format_options"
                                   `(* ,unsigned-int))))
    (lambda* (#:key
              (abbreviated-size DESCRIBE-FORMAT-ABBREVIATED-SIZE)
              (always-use-long-format? DESCRIBE-FORMAT-ALWAYS-USE-LONG-FORMAT?)
              (dirty-suffix DESCRIBE-FORMAT-DIRTY-SUFFIX))
      (let ((describe-format-options
             (make-describe-format-options-bytestructure)))
        (proc (describe-format-options->pointer describe-format-options)
              DESCRIBE-FORMAT-OPTIONS-VERSION)
        (set-describe-format-options-abbreviated-size! describe-format-options
                                                       abbreviated-size)
        (when always-use-long-format?
          (set-describe-format-options-always-use-long-format!
           describe-format-options 1))
        (when (> (string-length dirty-suffix) 0)
          (set-describe-format-options-dirty-suffix!
           describe-format-options (string->pointer dirty-suffix)))

        describe-format-options))))

(define describe-format
  (let ((proc (libgit2->procedure* "git_describe_format" '(* * *))))
    (lambda* (result #:optional (options (make-describe-format-options)))
      (let ((out (make-buffer)))
        (proc out (describe-result->pointer result)
              (describe-format-options->pointer options))
        (let ((out* (buffer-content/string out)))
          (free-buffer out)
          out*)))))
