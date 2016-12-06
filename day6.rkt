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

(define (rows->columns rows)
  (define width (length (first rows)))
  (for/list ((i (in-range width)))
    (map (lambda(row) (list-ref row i)) rows)))

(define (column->letter column sort-fn)
  (define freq-table (frequency-table column))
  
  (define sorted-freq (sort (hash->list freq-table) sort-fn #:key cdr))
  (define letter (caar sorted-freq))
  letter)

(define (error-correct messages sort-fn)
  (define frame
    (rows->columns
     (map string->list messages)))
  (list->string
   (map (lambda (column)
          (column->letter column sort-fn)) frame)))

(check-equal? (error-correct test-messages >) "easter" "Test (error-correct)")

(define messages (read-input "input6.txt"))
(define correct-part-1 (error-correct messages >))
(displayln correct-part-1)
(define correct-part-2 (error-correct messages <))
(displayln correct-part-2)
