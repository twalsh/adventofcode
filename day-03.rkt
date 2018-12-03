#lang racket
(require rackunit)
(require "advent-utils.rkt")

(struct claim (id x y height width) #:transparent)

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
      (define overlaps (hash-ref! claim-map key (set)))
      (hash-set! claim-map
                    key
                    (set-add overlaps (claim-id c)))))
  claim-map)

(define (claim-overlap claim-map)
  (for/sum ((p (hash-keys claim-map)))
    (if (> (set-count (hash-ref claim-map p)) 1)
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

(define (claim-overlaps? claim claim-map)
  (for/first ((p (hash-values claim-map))
              #:when (and (set-member? p (claim-id claim)) (> (set-count p) 1)))
    #t))

; Part Two
(define (find-non-overlapping-claim claims claim-map)
  (for/first ((c claims)
              #:when (not (claim-overlaps? c claim-map)))
    (claim-id c)))

(check-eq? (find-non-overlapping-claim test-claims test-claim-map) 3)

(define part-two (find-non-overlapping-claim claims claim-map))
(printf "Day 3. Part Two: ~a~n" part-two)