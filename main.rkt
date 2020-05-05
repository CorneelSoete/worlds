#lang racket

;world struct
(struct world
  (parent
   env)
  #:transparent)

(struct env
  ([read #:mutable]
   [wrote #:mutable])
  #:transparent)
(define (wrote w)
  (env-wrote (world-env w)))
(define (read w)
  (env-read (world-env w)))

(define (empty-env)
  (env (make-hash) (make-hash)))
(define (write! w var val)
  (hash-set! (wrote w) var val) val)
(define (wrote? w var)
  (hash-ref (wrote w) var #f))
(define (read! w var val)
  (hash-set! (read w) var val) val)
(define (read? w var)
  (hash-ref (read w) var #f))

;thisworld and sprout
(define globalworld (world #f (empty-env)))
(define thisworld globalworld)

(define (sprout w)
  (define child (world w (empty-env)))
  child)

(define (commit w)
  (let ((true? #t)
        (par (world-parent w)))
    (when par
      (begin
        (hash-for-each (read w)
                       (lambda (var val)
                         (when (not (or (eq? val (check-envs (world-parent w) var))
                                        (eq? 'notfound (check-envs (world-parent w) var))))
                           (set! true? #f))))
        (if true?
            (begin
              (hash-for-each (wrote w)
                             (lambda (var val)
                               (write! (world-parent w) var val)))
              (hash-for-each (read w)
                             (lambda (var val)
                               (read! (world-parent w) var val))))
            (error "illegal commit exception"))))))

;since this is a macro but it has to change the global, we have to define a function which will do the set! in its place
(define-syntax-rule (in world expr ...)
  (let ((prev-global thisworld))
    (replace-thisworld world)
    (define result (begin expr ...))
    (replace-thisworld prev-global)
    result))
(define (replace-thisworld newglobal)
  (set! thisworld newglobal))

;Look for the variable in the environment of the world and its parents
(define (check-env w var)
  (let ((wrote (wrote? w var)))
    (if wrote wrote
        (read? w var))))

(define (check-envs w var pos)
  (let* ((vec (check-env w var))
         (val (if (and pos (vector? vec))
                  (if (not (eq? (vector-ref vec pos) 'lookinpar)) vec #f)
                  vec))
         (par (world-parent w)))
    (if val val
        (if par
            (check-envs par var pos)
            'notfound))))
  

;find a variable in a world, if found. remember in the environment
(define (dlookup var . pos)
  (let* ((vec (check-env thisworld var))
         (pos (if (pair? pos)(car pos)#f))
         (val (cond ((and pos (vector? vec))
                     (vector-ref vec pos))
                    ((vector? vec)
                     (define vect (build-vector (vector-length vec)
                                               (lambda(x)(dlookup var x))))
                     (read! thisworld var vect))
                    (else vec)))
         (par (world-parent thisworld)))
    (if (and val (not (eq? val 'lookinpar))) val
        (begin
          (if par
              (set! val (check-envs (world-parent thisworld) var pos))
              (set! val 'notfound))
          (if (eq? val 'notfound)
              (displayln (format "var '~a' not found" var))
              (cond ((and pos (vector? vec))
                       (vector-set! vec pos (vector-ref val pos))
                       (vector-ref val pos))
                      ((and pos (vector? val))
                       (define length (vector-length val))
                       (set! val (vector-ref val pos))
                       (read! thisworld var (build-vector length (lambda(x)(if (eq? x pos) val 'lookinpar))))
                       val)
                      (else (read! thisworld var val))))))))

(define-syntax-rule (lookup var)
  (dlookup 'var))

;wdefine defines the variable and sets it in its environment
(define-syntax (wdefine VAR)
  (syntax-case VAR ()
    [( _ (a b ...) ... c) #'(write! thisworld 'a ... (lambda (b ...) c) ...)]
    [(_ a b) #'(write! thisworld 'a b)]))

;set! in a specific world, searches if one of children uses the variable. if so, set its dirty bit to #t
(define-syntax-rule (wset! VAR VAL)
  (write! thisworld 'VAR VAL))

;wvector procedures
(define-syntax-rule (wvector-ref vec pos)
  (dlookup 'vec pos))

(define-syntax-rule (wvector-set! vec pos val)
  (vector-set! (lookup vec) pos val))
  

(provide (all-from-out racket)
         in          sprout        commit
         lookup      wdefine       wset!
         wvector-ref wvector-set!
         globalworld   world-env
         replace-thisworld
         world world-parent env ;added for testing purposes
         )