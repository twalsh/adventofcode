#lang racket

(require "advent-utils.rkt")

(define test-messages (string-split 
"eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar" #:repeat? #t))

(define (rows->columns rows)
  (define width (length (first rows)))
  (for/list ((i (in-range width)))
    (map (lambda(row) (list-ref row i)) rows)))

(define frame (rows->columns (map string->list test-messages)))

(define (column->letter column)
  (define freq-table (frequency-table column))
  (displayln freq-table))

(column->letter (first frame))
