#lang s-exp worlds

;exception handling example
(define (do-it x y)
  (try-catch
   (lambda()
     (try-catch
      (lambda ()
        (logarithm x y))
      (lambda (exception)
        (eq? exception 'division-by-zero))
      (lambda (exception)
        (displayln "x/0"))))
   (lambda (exception)
     (eq? exception 'negative-log))
   (lambda(exception)
     (displayln "log(-x)"))))

(define (divide x y)
  (if (= y 0)
      (throw 'division-by-zero)
      (/ x y)))
(define (logarithm x y)
  (wset! r "changed") ;does a side effect (set testers to "changed")
  (if (or (<= x 0)
          (<= y 0))
      (throw 'negative-log)
      (divide (log x)(log y))))


; Actual exception handling code
(define (*throw* exception)
  (displayln (format "No exception handler: ~a" exception)))
(define (throw exception)
  (*throw* exception))

(define (try-catch try-lambda filter handler)
  (call/cc
   (lambda (cont)
     (define keep *throw*)
     (define child (sprout globalworld)) ;the world in which the side effects will happen
     (set! *throw* (lambda (exception)
                     (set! *throw* keep)
                     (if (filter exception)
                         (begin
                           (replace-thisworld globalworld)
                           (cont (handler exception)))
                         (throw exception))))
     (define result (in child (try-lambda))) ;if no exception is thrown result is set to the result of try-lambda
     (commit child) ;side effects are up-propagated to globalworld
     (set! *throw* keep)
     result)))

(require rackunit)
(require rackunit/text-ui)

(define exception-handling
  (test-suite
   "exception handling"
   (wdefine r "static")
   (test-case
    "r in globalworld should be 'static'"
    (check-equal? (lookup 'r) "static"))
   (do-it 5 0)
   (test-case
    "r in globalworld should still be 'static'"
    (check-equal? (lookup 'r) "static"))
   (do-it 5 5)
   (test-case
    "r in globalworld should now be 'changed'"
    (check-equal? (lookup 'r) "changed"))))

(run-tests exception-handling)