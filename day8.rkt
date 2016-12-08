#lang racket

(define cols 50)
(define rows 6)

(define screen (make-vector (* cols rows) #\.))
(define (print-screen)
  (for ((y (in-range rows)))
    (for ((x (in-range cols)))
      (define p (pos x y))
      ;(printf "~a ~a ~a~n" x y p)
      (printf "~a" (vector-ref screen p)))
    (newline)))

(define (pos x y)
  (+ x (* y cols)))

(define (rect a b)
  (for* ((x (in-range a))
         (y (in-range b)))
    (vector-set! screen (pos x y) #\#)))

(rect 3 2)
(print-screen)