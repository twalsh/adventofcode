#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (valid-triangle? sides)
  (match-let (((list a b c) sides))
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a))))

(check-equal? (valid-triangle? '(5 10 25)) #f "Invalid triangle")

(define puzzle-data (map (lambda (l) (map string->number (string-split l))) (read-input "input3.txt")))
;(displayln puzzle-data)

(count valid-triangle? puzzle-data)

(define vertical-triangles
  (let loop ((lines puzzle-data))
    (if (empty? lines)
        '()
        (cons (take lines 3) (loop (drop lines 3))))))

(define (transform rows)
  (for/list ((i (in-range 3)))
    (map (lambda (r) (list-ref r i)) rows)))

(define transformed-triangles (map transform vertical-triangles))

(for/sum ((triplet transformed-triangles))
  (count valid-triangle? triplet))
