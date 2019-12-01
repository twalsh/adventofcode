#lang racket

(require rackunit)
(require "advent-utils.rkt")
(require "advent7.rkt")

(define input-file "input7.txt")
(define lines (call-with-input-file input-file read-lines))

(define part-one (do-part-one lines))
(printf "Part One ~s~n" part-one)
(check-equal? part-one 956)

(define part-two (do-part-two part-one))
(printf "Part Two ~s~n" part-two)
(check-equal? part-one 40149)
