;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (git structs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select (null-pointer?
                                           bytevector->pointer
                                           make-pointer
                                           pointer->bytevector
                                           pointer->string))
  #:use-module (bytestructures guile)
  #:use-module (ice-9 match)
  #:export (git-error? git-error-code git-error-message git-error-class pointer->git-error
            time->pointer pointer->time time-time time-offset
            signature->pointer pointer->signature signature-name signature-email signature-when
            oid? oid->pointer pointer->oid make-oid-pointer oid=?

            diff-file? diff-file-oid diff-file-path diff-file-size diff-file-flags diff-file-mode diff-file-id-abbrev

            diff-delta? diff-delta-status diff-delta-flags diff-delta-status diff-delta-nfiles diff-delta-old-file diff-delta-new-file

            status-entry? status-entry-status status-entry-head-to-index status-entry-index-to-workdir pointer->status-entry

            make-status-options status-options->pointer set-status-options-show! set-status-options-flags!

            make-fetch-options fetch-options-bytestructure fetch-options->pointer fetch-options-callbacks
            set-fetch-options-callbacks! set-remote-callbacks-credentials!

            make-clone-options clone-options->pointer clone-options-fetch-options))


;;; bytestructures helper

(define bytestructure->pointer
  (compose bytevector->pointer bytestructure-bytevector))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure (pointer->bytevector pointer (bytestructure-descriptor-size struct))
                      0
                      struct))

;;; git-time

(define %time (bs:struct `((time ,int64) ;; time in seconds since epoch
                           (offset ,int)))) ;; timezone offset, in minutes

(define-record-type <time>
  (%make-time bytestructure)
  time?
  (bytestructure time-bytestructure))

(define (pointer->time pointer)
  (%make-time (pointer->bytestructure pointer %time)))

(define (time->pointer time)
  (bytestructure->pointer (time-bytestructure time)))

(define (time-time time)
  (bytestructure-ref (time-bytestructure time) 'time))

(define (time-offset time)
  (bytestructure-ref (time-bytestructure time) 'offset))

;;; git-signature

(define %signature (bs:struct `((name ,(bs:pointer uint8)) ;; char *
                                (email ,(bs:pointer uint8)) ;; char *
                                (when ,%time))))

(define-record-type <signature>
  (%make-signature bytestructure)
  signature?
  (bytestructure signature-bytestructure))

(define (pointer->signature pointer)
  (%make-signature (pointer->bytestructure pointer %signature)))

(define (signature->pointer signature)
  (bytestructure->pointer (signature-bytestructure signature)))

(define (signature-name signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'name))))

(define (signature-email signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'email))))

(define (signature-when signature)
  (let ((when* (bytestructure-ref (signature-bytestructure signature) 'when)))
    (%make-time when*)))

;;; git oid

(define-syntax GIT-OID-RAWSZ
  (identifier-syntax 20))

(define-record-type <oid>
  (%make-oid bytevector)
  oid?
  (bytevector oid-bytevector))

(define (pointer->oid pointer)
  ;; C functions typically return 'const git_oid *' and the OID's memory
  ;; belongs to the object it is associated with.  Thus, always copy the OID
  ;; contents to make sure it's not modified or freed behind our back.
  (%make-oid (bytevector-copy
              (pointer->bytevector pointer GIT-OID-RAWSZ))))

(define (oid->pointer oid)
  (bytevector->pointer (oid-bytevector oid)))

(define (make-oid-pointer)
  (bytevector->pointer (make-bytevector GIT-OID-RAWSZ)))

(define (oid=? oid1 oid2)
  "Return true if OID1 and OID2 are equal."
  ;; This is more efficient than calling 'git_oid_equal' through the FFI.
  (bytevector=? (oid-bytevector oid1)
                (oid-bytevector oid2)))

;;; git status options

(define %error
  (bs:struct `((message ,(bs:pointer uint8))
               (class   ,int))))

(define %strarray
  (bs:struct `((strings ,(bs:pointer
                          (bs:pointer uint8)))
               (count ,size_t))))

(define %status-options
  (bs:struct `((version ,unsigned-int)
               (status-show ,int)
               (flags ,unsigned-int)
               (pathspec ,%strarray))))

(define %diff-file
  (bs:struct `((oid ,(bs:vector 20 uint8))
               (path ,(bs:pointer uint8))
               (size ,int64)
               (flags ,uint32)
               (mode ,uint16)
               (id-abbrev ,uint16))))

(define %diff-delta
  (bs:struct `((status ,int)
               (flags ,uint32)
               (similarity ,uint16)
               (nfiles ,uint16)
               (old-file ,%diff-file)
               (new-file ,%diff-file))))

(define %status-entry
  (bs:struct `((status ,int)
               (head-to-index ,(bs:pointer %diff-delta))
               (index-to-workdir ,(bs:pointer %diff-delta)))))

(define (flags->symbols flags map-list)
  (fold (lambda (flag-map symbols)
          (match flag-map
            ((flag symbol)
             (if (> (logand flag flags) 0)
                 (cons symbol symbols)
                 symbols))))
        '()
        map-list))

(define (status-names status)
  (flags->symbols status
                  '((1     index-new)
                    (2     index-modified)
                    (4     index-deleted)
                    (8     index-renamed)
                    (16    index-typechange)
                    (128   wt-new)
                    (256   wt-modified)
                    (512   wt-deleted)
                    (1024  wt-typechange)
                    (2048  wt-renamed)
                    (4096  wt-unreadable)
                    (16384 ignored)
                    (32768 conflicted))))

(define-record-type <git-error>
  (%make-git-error code message class)
  git-error?
  (code    git-error-code)
  (message git-error-message)
  (class   git-error-class))

(define-record-type <diff-file>
  (%make-diff-file oid path size flags mode id-abbrev)
  diff-file?
  (oid diff-file-oid)
  (path diff-file-path)
  (size diff-file-size)
  (flags diff-file-flags)
  (mode diff-file-mode)
  (id-abbrev diff-file-id-abbrev))

(define-record-type <diff-delta>
  (%make-diff-delta status flags similarity nfiles old-file new-file)
  diff-delta?
  (status diff-delta-status)
  (flags diff-delta-flags)
  (similarity diff-delta-similarity)
  (nfiles diff-delta-nfiles)
  (old-file diff-delta-old-file)
  (new-file diff-delta-new-file))

(define-record-type <status-entry>
  (%make-status-entry status head-to-index index-to-workdir)
  status-entry?
  (status status-entry-status)
  (head-to-index status-entry-head-to-index)
  (index-to-workdir status-entry-index-to-workdir))

(define-record-type <status-options>
  (%make-status-options bytestructure)
  status-options?
  (bytestructure status-options-bytestructure))

(define* (pointer->git-error pointer code)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %error)))
        (%make-git-error code
                         (pointer->string
                          (make-pointer (bytestructure-ref bs 'message)))
                         (bytestructure-ref bs 'class)))))

(define (make-status-options)
  (%make-status-options (bytestructure %status-options)))

(define (status-options->pointer status-options)
  (bytestructure->pointer (status-options-bytestructure status-options)))

(define (set-status-options-show! status-options show)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'status-show show))

(define (set-status-options-flags! status-options flags)
  (bytestructure-set! (status-options-bytestructure status-options)
                      'flags flags))

(define (bs-diff-file->diff-file bs)
  (%make-diff-file
   (%make-oid (bytestructure-bytevector
               (bytestructure-ref bs 'oid)))
   (pointer->string
    (make-pointer (bytestructure-ref bs 'path)))
   (bytestructure-ref bs 'size)
   (bytestructure-ref bs 'flags)
   (bytestructure-ref bs 'mode)
   (bytestructure-ref bs 'id-abbrev)))

(define (pointer->diff-delta pointer)
  (if (null-pointer? pointer)
      #f
      (let ((bs (pointer->bytestructure pointer %diff-delta)))
        (%make-diff-delta
         (bytestructure-ref bs 'status)
         (bytestructure-ref bs 'flags)
         (bytestructure-ref bs 'similarity)
         (bytestructure-ref bs 'nfiles)
         (bs-diff-file->diff-file (bytestructure-ref bs 'old-file))
         (bs-diff-file->diff-file (bytestructure-ref bs 'new-file))))))

(define (pointer->status-entry pointer)
  (let ((bs (pointer->bytestructure pointer %status-entry)))
    (%make-status-entry
     (status-names (bytestructure-ref bs 'status))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'head-to-index)))
     (pointer->diff-delta
      (make-pointer (bytestructure-ref bs 'index-to-workdir))))))

;; git fetch options

(define %remote-callbacks
  (bs:struct `((version ,unsigned-int)
               (sideband-progress ,(bs:pointer uint8))
               (completion ,(bs:pointer uint8))
               (credentials ,(bs:pointer uint8))
               (certificate-check ,(bs:pointer uint8))
               (transfer-progress ,(bs:pointer uint8))
               (update-tips ,(bs:pointer uint8))
               (pack-progress ,(bs:pointer uint8))
               (push-transfer-progress ,(bs:pointer uint8))
               (push-update-reference ,(bs:pointer uint8))
               (push-negotiation ,(bs:pointer uint8))
               (transport ,(bs:pointer uint8))
               (payload ,(bs:pointer uint8)))))

(define %proxy-options
  (bs:struct `((version ,int)
               (type ,int)
               (url ,(bs:pointer uint8))
               (credentials ,(bs:pointer uint8))
               (certificate-check ,(bs:pointer uint8))
               (payload ,(bs:pointer uint8)))))

(define %fetch-options
  (bs:struct `((version ,int)
               (callbacks ,%remote-callbacks)
               (prune ,int)
               (update-fetchhead ,int)
               (download-tags ,int)
               (proxy-opts ,%proxy-options)
               (custom-headers ,%strarray))))

(define-record-type <fetch-options>
  (%make-fetch-options bytestructure)
  fetch-options?
  (bytestructure fetch-options-bytestructure))

(define (make-fetch-options)
  (%make-fetch-options (bytestructure %fetch-options)))

(define (fetch-options->pointer fetch-options)
  (bytestructure->pointer (fetch-options-bytestructure fetch-options)))

(define (fetch-options-callbacks fetch-options)
  (bytestructure-ref (fetch-options-bytestructure fetch-options) 'callbacks))

(define (set-fetch-options-callbacks! fetch-options callbacks)
  (bytestructure-set! (fetch-options-bytestructure fetch-options)
                      'callbacks callbacks))

(define (set-remote-callbacks-credentials! callbacks credentials)
  (bytestructure-set! callbacks 'credentials credentials))

;; git clone options

(define %checkout-options
  (bs:struct `((version ,unsigned-int)
               (checkout-strategy ,unsigned-int)
               (disable-filters ,int)
               (dir-mode ,unsigned-int)
               (file-mode ,unsigned-int)
               (file-open-flags ,int)
               (notify-flags ,unsigned-int)
               (notify-cb ,(bs:pointer uint8))
               (notify-payload ,(bs:pointer uint8))
               (progress-cb ,(bs:pointer uint8))
               (progress-payload ,(bs:pointer uint8))
               (paths ,%strarray)
               (baseline ,(bs:pointer uint8))
               (baseline-index ,(bs:pointer uint8))
               (target-directory ,(bs:pointer uint8))
               (ancestor-label ,(bs:pointer uint8))
               (our-label ,(bs:pointer uint8))
               (their-label ,(bs:pointer uint8))
               (perfdata-cb ,(bs:pointer uint8))
               (perfdata-payload ,(bs:pointer uint8)))))

(define %clone-options
  (bs:struct `((version ,int)
               (checkout-opts ,%checkout-options)
               (fetch-opts ,%fetch-options)
               (bare ,int)
               (local ,int)
               (checkout-branch ,(bs:pointer uint8))
               (repository-cb ,(bs:pointer uint8))
               (repository-cb-payload ,(bs:pointer uint8))
               (remote-cb ,(bs:pointer uint8))
               (remote-cb-payload ,(bs:pointer uint8)))))

(define-record-type <clone-options>
  (%make-clone-options bytestructure)
  clone-options?
  (bytestructure clone-options-bytestructure))

(define (make-clone-options)
  (%make-clone-options (bytestructure %clone-options)))

(define (clone-options->pointer clone-options)
  (bytestructure->pointer (clone-options-bytestructure clone-options)))

(define (clone-options-fetch-options clone-options)
  (%make-fetch-options
   (bytestructure-ref (clone-options-bytestructure clone-options) 'fetch-opts)))
