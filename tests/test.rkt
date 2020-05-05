#lang s-exp worlds

(require rackunit)
(require rackunit/text-ui)

;test for scoped side-effects
(define scoped-side-effects
  (test-suite
   "test for scoped side effects"
   (let ((w (sprout globalworld)))
     (wdefine r 0)
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (test-case
      "parent of w equal to globalworld"
      (check-equal? globalworld (world-parent w)))
     (in w
         (test-case
          "r in w should be 0"
          (check-equal? (lookup r) 0))
         (wset! r 1)
         (test-case
          "r in w should be 1"
          (check-equal? (lookup r) 1)))
     (test-case
      "r in globalworld should still be 0"
      (check-equal? (lookup r) 0))
     (in w
         (test-case
          "r in w should still be 1"
          (check-equal? (lookup r) 1))
         (wset! r 2)
         (test-case
          "r in w should now be 2"
          (check-equal? (lookup r) 2)))
     (check-equal? (lookup r) 0))))

;test to see if the top level commits are ignored
(define test-top-level-commit
  (test-suite
   "test for top level commit"
   (check-equal? (commit globalworld) (void) "void?")))

;test to see if the commits are working as they should
(define test-commit
  (test-suite
   "tests for committing"
   (let* ((parent (sprout globalworld))
          (child (sprout parent)))
     (wdefine r 0)
     (test-case
      "parent of child equal to parent"
      (check-equal? parent (world-parent child)))
     (in child
         (wset! r 1)
         (test-case
          "r in child should be 1"
          (check-equal? (lookup r) 1)))
     (in parent
         (test-case
          "r in parent should be 0"
          (check-equal? (lookup r) 0)))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (commit child)
     (in child
         (test-case
          "r in child should be 1"
          (check-equal? (lookup r) 1)))
     (in parent
         (test-case
          "r in parent should be 1"
          (check-equal? (lookup r) 1)))
     (test-case
      "r in globalworld should still be 0"
      (check-equal? (lookup r) 0)))))
     
;tests for commits when nothing is written and only read
(define test-commit-read-only
  (test-suite
   "tests for committing"
   (let* ((parent (sprout globalworld))
          (child (sprout parent)))
     (wdefine r 0)
     (test-case
      "parent of child equal to parent"
      (check-equal? parent (world-parent child)))
     (in child
         (test-case
          "r in child should be 0"
          (check-equal? (lookup r) 0)))
     (in parent
         (test-case
          "r in parent should be 0"
          (check-equal? (lookup r) 0)))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (commit child)
     (in child
         (test-case
          "r in child should still be 0"
          (check-equal? (lookup r) 0)))
     (in parent
         (test-case
          "r in parent should still be 0"
          (check-equal? (lookup r) 0)))
     (test-case
      "r in globalworld should still be 0"
      (check-equal? (lookup r) 0)))))

;test to see if the top world works fine
(define test-top-world
  (test-suite
   "tests for the top world"
   (wdefine r 0)
   (test-case
    "r in globalworld should be 0"
    (check-equal? (lookup r) 0))
   (wset! r 1)
   (test-case
    "r in globalworld should be 1"
    (check-equal? (lookup r) 1))))

;test committing to the top world
(define commit-to-top
  (test-suite
   "committing to top world"
   (let ((w (sprout globalworld)))
     (wdefine r 0)
     (in w (wset! r 1))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (commit w)
     (test-case
      "r in globalworld should be 1"
      (check-equal? (lookup r) 1)))))

;test serializability-check-failed
(define test-serializability-check-failed
  (test-suite
   "test if the serializability check fails"
   (let ((w (sprout globalworld)))
     (wdefine r 0)
     (in w
         (test-case
          "r in w should be 0"
          (lookup r)))
     (wset! r 1)
     (test-case
      "r in globalworld should be 1"
      (lookup 'r))
     (test-case
      "commit should throw an error"
      (check-exn exn:fail?
                 (lambda () (commit w)))))))

;test no surprises
(define test-no-surprises
  (test-suite
   "no surprises"
   (let ((w (sprout globalworld)))
     (wdefine r 0)
     (wset! r 1)
     (in w
         (test-case
          "r in w should be 1"
          (check-equal? (lookup r) 1)))
     (wset! r 2)
     (in w
         (test-case
          "r in w should still be 1"
          (check-equal? (lookup r) 1))))))

;test vectors
(define test-vectors
  (test-suite
   "vectors"
   (let ((w (sprout globalworld)))
     (wdefine r (make-vector 5 0))
     (in w
         (test-case
          "r0 should be 0 in w"
          (check-equal? (wvector-ref r 0) 0))
         (test-case
          "r should be a 5x1 vector"
          (check-equal? (lookup r) (make-vector 5 0)))
         (wvector-set! r 0 1)
         (test-case
          "r0 should be 1 in w"
          (check-equal? (wvector-ref r 0) 1)))
     (test-case
      "r0 should still be 0 in globalworld"
      (check-equal? (wvector-ref r 0) 0))
     (wvector-set! r 1 1)
     (in w
         (test-case
          "r1 should still be 0 in w"
          (check-equal? (wvector-ref r 1) 0)))
     ;test vector-ref in child when set in parent
     (wdefine r2 (make-vector 5 0))
     (in w (test-case
          "r0 should be 0 in w"
          (check-equal? (wvector-ref r2 0) 0)))
     (wvector-set! r2 0 1)
     (wvector-set! r2 1 1)
     (in w
         (test-case
          "r0 should still be 0 in w"
          (check-equal? (wvector-ref r2 0) 0))
         (test-case
          "r1 should now be 1 in w"
          (check-equal? (wvector-ref r2 1) 0))))))

;(run-tests scoped-side-effects)
;(run-tests test-top-level-commit)
;(run-tests test-commit)
;(run-tests test-commit-read-only)
;(run-tests test-top-world)
;(run-tests commit-to-top)
;(run-tests test-serializability-check-failed)
;(run-tests test-no-surprises)
(run-tests test-vectors)