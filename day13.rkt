#lang racket

(require data/bit-vector)
(require srfi/25)

(define (is-wall? x y fav-num)
  ;x*x + 3*x + 2*x*y + y + y*y
  (define sum
    (+ (* x x) (* 3 x) (* 2 x y) y (* y y)
       fav-num))
  (define bits (string->bit-vector (number->string sum 2)))
  (odd? (bit-vector-popcount bits)))
             
(define test-size 10)
(define test-num 10)

(define (make-grid size fav-num)
  (define grid (make-array (shape 0 size 0 size)))
  (for* ((x (in-range size))
         (y (in-range size)))
    (array-set! grid x y (is-wall? x y fav-num)))
  grid)

(define (print-grid grid)
  (define rows (array-end grid 0))
  (define cols (array-end grid 1))
  (for ((y (in-range rows)))
    (for ((x (in-range cols)))
      (define p (array-ref grid x y))
      (display (if p #\# #\space)))
    (newline))
  (newline))

(define test-grid (make-grid test-size test-num))
(print-grid test-grid)
