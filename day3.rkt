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