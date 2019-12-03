#lang racket

(require data/bit-vector)
(require rackunit)
; Conflicts with transpose from graph
(require (rename-in srfi/25 (transpose array:transpose)))

(require graph)

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

(define (make-edges grid)
  (define cols (array-end grid 1))
  (define rows (array-end grid 0))

  (foldl append '()
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
                                 ; Cannot move diagonally
                                 (not (and (= dx 1) (= dy 1)))
                                 ; Edges of grid
                                 (and (>= (+ x dx) 0) (< (+ x dx) cols)) 
                                 (and (>= (+ y dy) 0) (< (+ y dy) rows))
                                 ; Cannot connect to wall grid point
                                 (not (array-ref grid (+ x dx) (+ y dy)))))
               (list (cons x y) (cons (+ x dx) (+ y dy)))))
           ; (displayln edges)
           edges)))

(define (shortest-path graph v1 v2)
  (fewest-vertices-path graph v1 v2))

(define (find-path cols rows num start end)
  (define grid (make-grid cols rows num))
  (define edges (make-edges grid))
  (define graph (unweighted-graph/undirected edges))
  (shortest-path graph start end))

(define edges (make-edges test-grid))

(define test-graph (unweighted-graph/undirected edges))

(define test-path
  (find-path 10 10 test-num (cons 1 1) (cons 7 4)))

(define test-moves (sub1 (length test-path)))
(check-equal? test-moves 11 "Test OK")

; Part 1
(define puzzle-path (find-path 41 41 1350 (cons 1 1) (cons 31 39)))
(displayln puzzle-path)
(sub1 (length puzzle-path))
; Part 2
(define puzzle-grid (make-grid 41 41 1350))
(define puzzle-edges (make-edges puzzle-grid))
(define puzzle-graph (unweighted-graph/undirected puzzle-edges))
(define-values (distance-map _) (dijkstra puzzle-graph (cons 1 1)))

(length
 (filter (λ (v)(<= v 50)) (hash-values distance-map)))