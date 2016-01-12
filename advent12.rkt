#lang racket

(require json)
(require rackunit)

(define input (with-input-from-file "input12.txt" (lambda () (read-json))))

(define (process-list in)
  (cond ((list? in) (for-each process-list in))
        ((hash? in) (for-each process-list (hash-values in)))
        (else in)))

(process-list input)