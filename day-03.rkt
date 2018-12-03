#lang racket
(require rackunit)
(require "advent-utils.rkt")

(struct claim (n x y height width) #:transparent)

(define (line->claim line)
  (match-let (
              [ (regexp #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"
                        (list _ n x y width height)) line])
    (claim (string->number n)
           (string->number x)
           (string->number y)
           (string->number height)
           (string->number width))))

(define (make-claim-map claims)
  (define claim-map (make-hash))
  (for ((c claims))
    (define x0 (claim-x c))
    (define y0 (claim-y c))
    (for* ((x (in-range x0 (+ x0 (claim-width c))))
           (y (in-range y0 (+ y0 (claim-height c)))))
      (define key (cons y x)) 
      (hash-update! claim-map key add1 0)))
  claim-map)

(define (claim-overlap claim-map)
  (for/sum ((p (hash-keys claim-map)))
    (if (> (hash-ref claim-map p) 1)
        1
        0)))

(define test-lines
  '("#1 @ 1,3: 4x4"
    "#2 @ 3,1: 4x4"
    "#3 @ 5,5: 2x2"))

(define test-claims (map line->claim test-lines))
(define test-claim-map (make-claim-map test-claims))
(check-eq? (claim-overlap test-claim-map) 4)

(define input-lines (read-input "input-03.txt"))
(define claims (map line->claim input-lines))
(define claim-map (make-claim-map claims))
(printf "Day 3. Part One: Claim overlap ~a ~n" (claim-overlap claim-map))

