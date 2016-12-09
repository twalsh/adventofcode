#lang racket

(require rackunit)
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

(define (column->letter column sort-fn)
  (define freq-table (frequency-table column))
  (define sorted-freq (sort (hash->list freq-table) sort-fn #:key cdr))
  (caar sorted-freq))

(define (error-correct messages sort-fn)
  (define frame
    (rows->columns
     (map string->list messages)))
  (define letters
    (for/list ((column frame))
      (column->letter column sort-fn)))
  (list->string letters))

(check-equal? (error-correct test-messages >) "easter" "Test (error-correct)")

(define messages (read-input "input6.txt"))
(define correct-part-1 (error-correct messages >))
(displayln correct-part-1)
(define correct-part-2 (error-correct messages <))
(displayln correct-part-2)
