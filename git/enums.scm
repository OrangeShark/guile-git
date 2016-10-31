(define-module (git enums))


(define-public GIT-TREEWALK-PRE 0)
(define-public GIT-TREEWALK-POST 1)

(define-public GIT-BRANCH-LOCAL 1)
(define-public GIT-BRANCH-REMOTE 2)
(define-public GIT-BRANCH-ALL (logior GIT-BRANCH-LOCAL GIT-BRANCH-REMOTE))
