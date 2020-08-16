#lang s-exp worlds

;actual undo code
(define undo-stack (list thisworld))

(define-syntax-rule (do expr ...)
  (let ((world (bigbang (car undo-stack))))
    (in world expr ...)
    (set! undo-stack (cons world undo-stack))
    (replace-thisworld world)))

(define (undo)
  (when (not (null? (cdr undo-stack)))
    (set! undo-stack (cdr undo-stack))
    (replace-thisworld (car undo-stack))))

(define (flattenHistory)
  (let loop ((world (car undo-stack))
             (rest (cdr undo-stack)))
    (when (not (null? rest))
      (collapse world)
      (loop (car rest)(cdr rest)))
    (set! undo-stack (list world))))

;undo example
(do (wdefine counter 0))
(do (wset! counter 5))
(do (wset! counter 2))
(undo)                             ;undo previous do
(do (displayln (lookup counter)))  ;displays 5
(flattenHistory)