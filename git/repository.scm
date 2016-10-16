
(define-module (git repository)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (repository-config
            repository-config-snapshot
            repository-detach-head
            repository-discover
            repository-free
            repository-get-namespace
            repository-head
            repository-head-detached?
            repository-head-unborn?
            repository-ident
            repository-index
            repository-init
            repository-is-bare?
            repository-is-empty?
            repository-is-shallow?
            repository-open
            repository-path
            repository-refdb
            repository-set-ident
            repository-state
            repository-workdir))

;;; repository

(define repository-config
  (let ((proc (libgit2->procedure* "git_repository_config" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->config (dereference-pointer out))))))

(define repository-config-snapshot
  (let ((proc (libgit2->procedure* "git_repository_config_snapshot" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->config (dereference-pointer out))))))

(define repository-detach-head
  (let ((proc (libgit2->procedure* "git_repository_detach_head" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-discover
  (let ((proc (libgit2->procedure* "git_repository_discover" `(* * ,int *))))
    (lambda (start-path across-fs ceiling-dirs)
      (let ((out (make-buffer)))
	(proc out
	      (string->pointer start-path)
	      (if across-fs 1 0)
	      (string->pointer ceiling-dirs))
	(let ((out* (buffer-content/string out)))
	  (free-buffer out)
	  out)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_fetchhead_foreach

(define repository-free
  (let ((proc (libgit2->procedure void "git_repository_free" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-get-namespace
  (let ((proc (libgit2->procedure '* "git_repository_get_namespace" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_hashfile

(define repository-head
  (let ((proc (libgit2->procedure* "git_repository_head" '(* *))))
    (lambda (repository)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository))
	(pointer->reference (dereference-pointer out))))))

(define repository-head-detached?
  (let ((proc (libgit2->procedure int "git_repository_head_detached" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
	((0) #f)
	((1) #t)
	(else => (lambda (code) (throw 'git-error code)))))))

(define repository-head-unborn?
  (let ((proc (libgit2->procedure int "git_repository_head_unborn" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
	((0) #f)
	((1) #t)
	(else => (lambda (code) (throw 'git-error code)))))))

(define repository-ident
  (let ((proc (libgit2->procedure* "git_repository_ident" '(* * *))))
    (lambda (repository)
      (let* ((name (make-bytevector (sizeof '*)))
             (name* ((bytevector->pointer name)))
             (email (make-bytevector (sizeof '*)))
             (email* ((bytevector->pointer email))))
	(proc name email (repository->pointer repository))
	(values (pointer->string (make-pointer (u64vector-ref name 0)))
		(pointer->string (make-pointer (u64vector-ref email 0))))))))

(define repository-index
  (let ((proc (libgit2->procedure* "git_repository_index" '(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc (repository->pointer repository))
	(pointer->index (dereference-pointer out))))))

(define repository-init
  (let ((proc (libgit2->procedure* "git_repository_init" `(* * ,int))))
    (lambda* (path #:optional (is-bare #f))
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (string->pointer path) (if is-bare 1 0))
	(pointer->repository (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_init_ext

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_init_init_options

(define repository-is-bare?
  (let ((proc (libgit2->procedure int "git_repository_is_bare" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-empty?
  (let ((proc (libgit2->procedure int "git_repository_is_empty" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-shallow?
  (let ((proc (libgit2->procedure int "git_repository_is_shallow" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_mergehead_foreach

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_message

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_message_remove

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_new

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_odb

(define repository-open
  (let ((proc (libgit2->procedure* "git_repository_open" '(* *))))
    (lambda (file)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (string->pointer file))
	(pointer->repository (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_open_baer

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_open_ext

(define repository-path
  (let ((proc (libgit2->procedure '* "git_repository_path" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

(define repository-refdb
  (let ((proc (libgit2->procedure* "git_repository_refdb" `(* *))))
    (lambda (repository)
      (let ((out ((bytevector->pointer (make-bytevector (sizeof '*))))))
	(proc out (repository->pointer repository))
	(pointer->refdb (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_reinit_filesystem

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_bare

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_config

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head_detached

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head_detached_from_annotated

(define repository-set-ident
  (let ((proc (libgit2->procedure* "git_repository_set_ident" '(* * *))))
    (lambda (repository name email) ;;; FIXE: make name and email optional
      (proc (repository->pointer repository)
	    (string->pointer name "UTF-8")
	    (string->pointer email "UTF-8")))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_index

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_namespace

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_odb

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_refdb

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_workdir

(define repository-state
  (let ((proc (libgit2->procedure int "git_repository_state" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_state_cleanup

(define repository-workdir
  (let ((proc (libgit2->procedure '* "git_repository_workdir" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_wrap_odb
