#lang racket

(require json)
(require rackunit)

(define input (with-input-from-file "input12.txt" (lambda () (read-json))))

(define (sum-js in)
  (apply + (flatten (cond ((list? in) (map sum-js in))
        ((hash? in) (map sum-js (hash-values in)))
        (else
         (if (number? in)
             in
             '()))))))

(define part-one (sum-js input))

(printf "Day 12. Part One: ~s~n" part-one)
(check-equal? part-one 156366)