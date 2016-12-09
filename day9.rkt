#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (parse-marker marker)
  (define (not-x? c) (not (char=? c #\x)))
  (define repeat (string->number (list->string (takef marker not-x?))))
  (define length (string->number (list->string (rest (dropf marker not-x?)))))
  (values length repeat))

(define (repeat-character characters length repeat)
  (cond ((= repeat 0) '())
        (else
         (define repeated (take characters length))
         (append repeated (repeat-character characters length (sub1 repeat))))))

(define (read-marker characters (marker '()))
  (define next (first characters))
  (cond ((eq? next #\))
         (define-values (length repeat) (parse-marker marker))
         (define repeated-characters (repeat-character (rest characters) length repeat))
        
         (append repeated-characters (read-data (drop characters (add1 length)))))
        (else 
         (read-marker (rest characters) (cons next marker)))))
  
(define (read-data characters)
  (cond ((empty? characters) '())
        (else 
         (define next (first characters))
         (case next
           ((#\()
            (read-marker (rest characters)))
           (else (cons next (read-data (rest characters))))))))

(define (decode data)
  (define characters (string->list data))
  (list->string (read-data characters)))

(define test-data
  '(("ADVENT" 6)
    ("A(1x5)BC" 7)
    ("(3x3)XYZ" 9)
    ("A(2x2)BCD(2x2)EFG" 11)
    ("(6x1)(1x3)A" 6)
    ("X(8x2)(3x3)ABCY" 18)))

(for ((item test-data))
  (define decoded (decode (car item)))
  (define item-length (string-length decoded))
  (displayln decoded)
  (check-equal? item-length (second item)))

(define puzzle-data (read-input "input9.txt"))
(define encrypted-file (first puzzle-data))
encrypted-file

(define puzzle-output (decode encrypted-file))
(displayln puzzle-output)

(string->list encrypted-file)

