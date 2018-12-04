#lang racket
(require gregor)
(require rackunit)

(require "advent-utils.rkt")

(define line-sorter (lambda (a b) (string<? (substring a 1 17) (substring b 1 17))))


(define test-lines (read-input "test-04.txt"))
(define sorted-test-lines (sort test-lines line-sorter))

(define lines (read-input "input-04.txt"))
(define sorted-lines (sort lines line-sorter))

(substring (first sorted-lines) 1 17)
(iso8601->time (substring (first sorted-lines) 12 17))
;(define (line->record line)
 ; (match-let (
  ;            [ (regexp #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"
   ;                     (list _ n x y width height)) line])
    ;(claim (string->number n)
     ;      (string->number x)
      ;     (string->number y)
       ;;    (string->number height)
         ;  (string->number width)))) 