#lang s-exp worlds

(require rackunit)
(require rackunit/text-ui)

;test for scoped side-effects
(define scoped-side-effects
  (test-suite
   "test for scoped side effects"
   (let ((w (bigbang globalworld)))
     (wdefine r 0)
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (test-case
      "parent of w equal to globalworld"
      (check-equal? globalworld (parent w)))
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

;test to see if the top level collapses are ignored
(define test-top-level-collapse
  (test-suite
   "test for top level collapse"
   (check-equal? (collapse globalworld) (void) "void?")))

;test to see if the collapses are working as they should
(define test-collapse
  (test-suite
   "tests for collapseting"
   (let* ((world (bigbang globalworld))
          (child (bigbang world)))
     (wdefine r 0)
     (test-case
      "parent of child equal to world"
      (check-equal? world (parent child)))
     (in child
         (wset! r 1)
         (test-case
          "r in child should be 1"
          (check-equal? (lookup r) 1)))
     (in world
         (test-case
          "r in world should be 0"
          (check-equal? (lookup r) 0)))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (collapse child)
     (in child
         (test-case
          "r in child should be 1"
          (check-equal? (lookup r) 1)))
     (in world
         (test-case
          "r in world should be 1"
          (check-equal? (lookup r) 1)))
     (test-case
      "r in globalworld should still be 0"
      (check-equal? (lookup r) 0)))))
     
;tests for collapses when nothing is written and only read
(define test-collapse-read-only
  (test-suite
   "tests for collapseting"
   (let* ((world (bigbang globalworld))
          (child (bigbang world)))
     (wdefine r 0)
     (test-case
      "parent of child equal to world"
      (check-equal? world (parent child)))
     (in child
         (test-case
          "r in child should be 0"
          (check-equal? (lookup r) 0)))
     (in world
         (test-case
          "r in world should be 0"
          (check-equal? (lookup r) 0)))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (collapse child)
     (in child
         (test-case
          "r in child should still be 0"
          (check-equal? (lookup r) 0)))
     (in world
         (test-case
          "r in world should still be 0"
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

;test collapsing to the top world
(define collapse-to-top
  (test-suite
   "collapseting to top world"
   (let ((w (bigbang globalworld)))
     (wdefine r 0)
     (in w (wset! r 1))
     (test-case
      "r in globalworld should be 0"
      (check-equal? (lookup r) 0))
     (collapse w)
     (test-case
      "r in globalworld should be 1"
      (check-equal? (lookup r) 1)))))

;test serializability-check-failed
(define test-serializability-check-failed
  (test-suite
   "test if the serializability check fails"
   (let ((w (bigbang globalworld)))
     (wdefine r 0)
     (in w
         (test-case
          "r in w should be 0"
          (check-equal? (lookup r) 0)))
     (wset! r 1)
     (test-case
      "r in globalworld should be 1"
      (check-equal? (lookup r) 1))
     (test-case
      "collapse should throw an error"
      (check-exn exn:fail?
                 (lambda () (collapse w)))))))

;test no surprises
(define test-no-surprises
  (test-suite
   "no surprises"
   (let ((w (bigbang globalworld)))
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
   (let ((w (bigbang globalworld)))
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
          (check-equal? (wvector-ref r2 1) 1))))))

;test pairs
(define test-mpairs
  (test-suite
   "mpairs"
   (let ((w (bigbang globalworld)))
     (wdefine r (mcons 0 0))
     (in w
         (test-case
          "car r should be 0 in w"
          (check-equal? (wmcar r) 0))
         (test-case
          "r should be a (0.0) mpair"
          (check-equal? (lookup r) (mcons 0 0)))
         (wset-mcar! r 1)
         (test-case
          "car r should be 1 in w"
          (check-equal? (wmcar r) 1)))
     (test-case
      "car r should still be 0 in globalworld"
      (check-equal? (wmcar r) 0))
     (wset-mcdr! r 1)
     (in w
         (test-case
          "cdr r should still be 0 in w"
          (check-equal? (wmcdr r) 0)))
     ;test vector-ref in child when set in parent
     (wdefine r2 (mcons 0 0))
     (in w (test-case
          "car r should be 0 in w"
          (check-equal? (wmcar r2) 0)))
     (wset-mcar! r2 1)
     (wset-mcdr! r2 1)
     (in w
         (test-case
          "car r should still be 0 in w"
          (check-equal? (wmcar r2) 0))
         (test-case
          "cdr r should now be 1 in w"
          (check-equal? (wmcdr r2) 1))))))

(run-tests scoped-side-effects)
(run-tests test-top-level-collapse)
(run-tests test-collapse)
(run-tests test-collapse-read-only)
(run-tests test-top-world)
(run-tests collapse-to-top)
(run-tests test-serializability-check-failed)
(run-tests test-no-surprises)
(run-tests test-vectors)
(run-tests test-mpairs)