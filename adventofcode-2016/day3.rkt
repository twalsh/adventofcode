#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (valid-triangle? sides)
  (match-let (((list a b c) sides))
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a))))

(check-equal? (valid-triangle? '(5 10 25)) #f "Invalid triangle")

(define puzzle-data (read-table "input3.txt"))
(count valid-triangle? puzzle-data)

(define vertical-triangles
  (let loop ((lines puzzle-data))
    (if (empty? lines)
        '()
        (cons (take lines 3) (loop (drop lines 3))))))

(define (rows->columns row)
  (for/list ((i (in-range 3)))
    (for/list ((triplet row))
      (list-ref triplet i))))

(define transformed-triangles (map rows->columns vertical-triangles))

(for/sum ((triplet transformed-triangles))
  (count valid-triangle? triplet))
