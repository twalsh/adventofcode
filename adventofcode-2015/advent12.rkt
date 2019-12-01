#lang racket

(require json)
(require rackunit)

(define input (with-input-from-file "input12.txt" (lambda () (read-json))))

(define (sum-js input [ pred? (lambda (h) #t) ])
  (define (fn in)
    (apply +
           (flatten
            (cond ((list? in) (map fn in))
                  ((and (hash? in) (pred? in))
                   (map fn (hash-values in)))
                  (else
                   (if (number? in)
                       in
                       '()))))))
  (fn input))

(define part-one (sum-js input))
(printf "Day 12. Part One: ~s~n" part-one)
(check-equal? part-one 156366)

(define part-two (sum-js input (lambda (h) (not (member "red" (hash-values h))))))
(printf "Day 12. Part Two: ~s~n" part-two)
(check-equal? part-two 96852)
