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

(define (make-grid rows cols fav-num)
  (define grid (make-array (shape 0 rows 0 cols)))
  (for* ((x (in-range rows))
         (y (in-range cols)))
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

(define test-grid (make-grid test-size test-size test-num))
(print-grid test-grid)

(struct edge (v1 v2) #:transparent)
(struct vertex (x y) #:transparent)

(define (make-edges grid)
  (define cols (array-end grid 1))
  (define rows (array-end grid 0))
  (for*/list ((x (in-range cols))
              (y (in-range rows))
              ; Filter out wall grid points
              #:when (not (array-ref grid x y)))
    ; Create list of vertices to which this vertex connects
    (define edges
      (for*/list ((dx '(0 1))
                  (dy '(0 1))
                  #:when (and
                          ; Vertex cannot connect to self
                          (not (and (= dx 0) (= dy 0)))
                          ; Edges of grid
                          (and (>= (+ x dx) 0) (< (+ x dx) cols)) 
                          (and (>= (+ y dy) 0) (< (+ y dy) rows))
                          ; Cannot connect to wall grid point
                          (not (array-ref grid (+ x dx) (+ y dy)))))
        (list (vertex x y) (vertex (+ x dx) (+ y dy)))))
    (displayln edges)
    edges))

(define edges (make-edges test-grid))

