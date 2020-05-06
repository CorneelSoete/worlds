#lang racket

;world struct
(struct world
  (parent
   env)
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  environment struct and procs  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;--- pretty env print ---
(define (dprint-env name world)
  (define rd
    (hash-map (read world)
              (lambda (x y)
                (if (composite? y)(cons x (composite-val y))(cons x y)))))
  (define wt
    (hash-map (wrote world)
              (lambda (x y)
                (if (composite? y)(cons x (composite-val y))(cons x y)))))
  (displayln (format "--- Environment of world ~a ---" name))
  (displayln (format "Read: ~a" rd))
  (displayln (format "Wrote: ~a" wt))
  (displayln "------------------------------"))
(define-syntax-rule (print-env w)
  (dprint-env 'w w))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  composite data structures  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct composite
  (accessor
   mutator
   build
   new
   [val #:mutable])
  #:transparent)

(define (access comp pos)
  ((composite-accessor comp) pos))
(define (mutate! comp pos val)
  ((composite-mutator comp) pos val))
(define (build comp var)
  ((composite-build comp) var))
(define (new-comp comp pos)
  ((composite-new comp) pos))
(define (value comp)
  (composite-val comp))

; create the correct composite data struct when needed
(define (compound val)
  (cond ((vector? val)(my-vect val))
        ((mpair? val) (my-pair val))
        (else val)))

; --- implementation of a vector data-structure ---
(define (my-vect val)
  (composite
   (lambda (pos)     (vector-ref val pos))
   (lambda (pos x)   (vector-set! val pos x))
   (lambda (var)     (my-vect (build-vector (vector-length val)
                                            (lambda(x)(dlookup var x)))))
   (lambda (pos)     (my-vect (build-vector (vector-length val)
                                            (lambda(x)(if (eq? x pos) (vector-ref val pos) 'lookinpar)))))
   val))

; --- implementation of a pair data-structure ---
(define (my-pair val)
  (composite
   (lambda (pos)     (pos val))
   (lambda (pos x)   (cond ((eq? pos car)(set-mcar! val x))
                           ((eq? pos cdr)(set-mcdr! val x))))
   (lambda (var)     (my-pair (mcons (dlookup var mcar)(dlookup var mcdr))))
   (lambda (pos)     (my-pair (cond ((eq? pos mcar)(mcons (mcar val) 'lookinpar))
                                     ((eq? pos mcdr)(mcons 'lookinpar (mcdr val))))))
   val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;   USER ACCESSIBLE PROCEDURES AND VARIABLES   ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define globalworld (world #f (empty-env)))
(define thisworld globalworld)

(define (bigbang w)
  (define child (world w (empty-env)))
  child)

(define (collapse w)
  (let ((true? #t)
        (par (world-parent w)))
    (when par
      (begin
        (hash-for-each (read w)
                       (lambda (var val)
                         (when (not (or (eq? val (check-envs (world-parent w) var #f))
                                        (eq? 'notfound (check-envs (world-parent w) var #f))))
                           (set! true? #f))))
        (if true?
            (begin
              (hash-for-each (wrote w)
                             (lambda (var val)
                               (write! (world-parent w) var val)))
              (hash-for-each (read w)
                             (lambda (var val)
                               (read! (world-parent w) var val))))
            (error "illegal collapse exception"))))))

;since this is a macro but it has to change thisworld, we have to define a function which will do the set! in its place
(define-syntax-rule (in world expr ...)
  (let ((prev-global thisworld))
    (replace-thisworld world)
    (define result (begin expr ...))
    (replace-thisworld prev-global)
    result))
(define (replace-thisworld newglobal)
  (set! thisworld newglobal))

;Lookup helper function which will look in in all the adjacent environments of a world
(define (check-envs w var pos)
  (let* ((wrote (wrote? w var))
         (read (read? w var))
         (val (cond
                ((and pos (composite? wrote)(not (eq? (access wrote pos) 'lookinpar))) wrote)
                ((and pos (composite? read)(not (eq? (access read pos) 'lookinpar))) read)
                ((and pos (or (composite? read)(composite wrote))) #f)
                (wrote wrote)
                (else read)))
         (par (world-parent w)))
    (if val val
        (if par
            (check-envs par var pos)
            'notfound))))
  

;find a variable in a world, if found. remember in the environment
(define (dlookup var . pos)
  (let* ((wrote (wrote? thisworld var))
         (read (read? thisworld var))
         (comp (if wrote wrote read))
         (pos (if (pair? pos)(car pos) #f))
         (val (cond ((and pos (composite? wrote)(not (eq? (access wrote pos) 'lookinpar)))
                     (access wrote pos))
                    ((and pos (composite? read)(not (eq? (access read pos) 'lookinpar)))
                     (access read pos))
                    ((and pos (or (composite? read)(composite? wrote))) 'lookinpar)
                    ((composite? comp)
                     (define com (build comp var))
                     (read! thisworld var com))
                    (else comp)))
         (par (world-parent thisworld)))
    (if (and val (not (eq? val 'lookinpar)))
        (if (composite? val)(value val)val)
        (begin
          (if par
              (set! val (check-envs (world-parent thisworld) var pos))
              (set! val 'notfound))
          (if (eq? val 'notfound)
              (displayln (format "var '~a' not found" var))
              (cond ((and pos (composite? comp))
                     (mutate! comp pos (access val pos))
                     (access val pos))
                    ((and pos (composite? val))
                     (read! thisworld var (new-comp val pos))
                     (access val pos))
                    (else (read! thisworld var val))))))))

(define-syntax-rule (lookup var)
  (dlookup 'var))

;wdefine defines the variable and sets it in its environment
(define-syntax (wdefine VAR)
  (syntax-case VAR ()
    [( _ (a b ...) ... c) #'(write! thisworld 'a ... (lambda (b ...) c) ...)]
    [(_ a b) #'(write! thisworld 'a (compound b))]))

;set! in a specific world, searches if one of children uses the variable. if so, set its dirty bit to #t
(define-syntax-rule (wset! VAR VAL)
  (write! thisworld 'VAR (compound VAL)))

;wvector procedures
(define-syntax-rule (wvector-ref vec pos)
  (dlookup 'vec pos))

(define (dvector-set! vec pos val)
  (let* ((wrote (wrote? thisworld vec))
         (read (read? thisworld vec))
         (length
          (vector-length (cond (wrote (value wrote))
                               (read (value read))
                               (else (check-envs (world-parent thisworld) vec pos))))))
    (if wrote (vector-set! (value wrote) pos val)
        (write! thisworld vec
                (my-vect (build-vector length (lambda(x)(if (eq? x pos) val 'lookinpar))))))))

(define-syntax-rule (wvector-set! vec pos val)
  (dvector-set! 'vec pos val))

;wmpairs procedures
(define-syntax-rule (wmcar pair)
    (dlookup 'pair mcar))
(define-syntax-rule (wmcdr pair)
  (dlookup 'pair mcdr))

(define-syntax-rule (dset-mcar! pair val)
  (let* ((wrote (wrote? thisworld pair))
         (read (read? thisworld pair)))
    (if wrote (set-mcar! (value wrote) val)
        (write! thisworld pair (my-pair (mcons val 'lookinpar))))))
(define-syntax-rule (wset-mcar! pair val)
  (dset-mcar! 'pair val))

(define-syntax-rule (dset-mcdr! pair val)
  (let* ((wrote (wrote? thisworld pair))
         (read (read? thisworld pair)))
    (if wrote (set-mcdr! (value wrote) val)
        (write! thisworld pair (my-pair (mcons 'lookinpar val))))))
(define-syntax-rule (wset-mcdr! pair val)
  (dset-mcdr! 'pair val))

  

(provide (all-from-out racket)
         in          bigbang       collapse
         lookup      wdefine       wset!
         wmcar   wset-mcar!
         wmcdr   wset-mcdr!
         wvector-ref   wvector-set!
         globalworld
         replace-thisworld
         print-env
         (rename-out [world-parent parent]))