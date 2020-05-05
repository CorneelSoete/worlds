#lang racket

;world struct
(struct world
  (parent
   env
   [children #:mutable]
   [dirty #:mutable])
  #:transparent)


;thisworld and sprout
(define thisworld (world #f (make-hash) '() #f))
(define globalworld thisworld)

(define (sprout w)
  (define child (world w (make-hash) '() #f))
  (set-world-children! w (cons child (world-children w)))
  child)
(define (commit w)
  (when (not (world-dirty w))
    (hash-for-each (world-env w)
                   (lambda (var val)
                     (hash-set! (world-env (world-parent w)) var val)))))

;since this is a macro but it has to change the global, we have to define a function which will do the set! in its place
(define-syntax-rule (in world expr)
  (let ((prev-global globalworld))
    (replace-global world)
    (define result expr)
    (replace-global prev-global)
    result))
(define (replace-global newglobal)
  (set! globalworld newglobal))

;Look for the variable in the environment of the world and its parents
(define (check-env w var)
  (define par (world-parent w))
  (let ((val (hash-ref (world-env w) var #f)))
    (if val val
        (if par
            (check-env par var)
            (displayln (format "var '~a' not found" var))))))

;find a variable in a world, if found. remember in the environment
(define (lookup var)
  (define val (check-env globalworld var))
  (hash-set! (world-env globalworld) var val)
  val)

;wdefine defines the variable and sets it in its environment
(define-syntax-rule (wdefine* VAR VAL)
  (begin
    (hash-set! (world-env globalworld) 'VAR VAL)))

(define-syntax (wdefine VAR)
  (syntax-case VAR ()
    [( _ (a b ...) ... c) #'(hash-set! (world-env globalworld) 'a ... (lambda (b ...) c) ...)]
    [(_ a b) #'(hash-set! (world-env globalworld) 'a b)]))

;set! in a specific world, searches if one of children uses the variable. if so, set its dirty bit to #t
(define-syntax-rule (wset! VAR VAL)
  (begin
    (for-each (lambda (w)
                (when (hash-ref (world-env w) 'VAR)
                  (set-world-dirty! w #t)))
              (world-children globalworld))
    (hash-set! (world-env globalworld) 'VAR VAL)))

(provide (all-from-out racket)
         in          sprout        commit
         lookup      wdefine       wset!
         thisworld
         replace-global
         world-env)