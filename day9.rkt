#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (parse-marker marker)
  (define (not-x? c) (not (char=? c #\x)))
  (define length (string->number (list->string (takef marker not-x?))))
  (define repeat (string->number (list->string (rest (dropf marker not-x?)))))
  (values length repeat))

(define (repeat-character characters length repeat)
  (cond ((= repeat 0) '())
        (else
         (define repeated (take characters length))
         (append repeated (repeat-character characters length (sub1 repeat))))))

(define (read-marker characters (marker '()))
  (define next (first characters))
  (cond ((eq? next #\))
         (define-values (length repeat) (parse-marker (reverse marker)))
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
  (check-equal? item-length (second item)))

(define puzzle-data (read-input "input9.txt"))
(define encrypted-file (first puzzle-data))

(define puzzle-output (string-length (decode encrypted-file)))
(displayln puzzle-output)

;;; Part two
(define (read-marker2 characters)
  (define text
    (let loop ((characters (rest characters)))
      (if (char=? (first characters) #\))
          '()
          (cons (first characters) (loop (rest characters))))))
  
  (define-values (repeat-length repeat-times) (parse-marker text))
  (marker text (+ (length text) 2) repeat-length repeat-times))

(struct marker (text length repeat-length repeat-times) #:transparent)

(define (read-data2 characters [sumc 0])
  ;(printf "read-data ~a ~a~n" characters sumc)
  (cond ((empty? characters)
       ;  (printf "return: ~a~n" sumc)
         sumc)
        (else 
         (define next (first characters))
         (case next
           ((#\()
            (define marker (read-marker2 characters))
         ;   (displayln marker)
            ; Get the string with marker removed
            (define tail (drop characters (marker-length marker)))
          ;  (printf "tail ~a~n" tail)
            (define repeat (marker-repeat-length marker))
            (define repeat-times (marker-repeat-times marker))
            (define head (take tail repeat))
         
            (define head-length (read-data2 head))
            (define head-repeat-length (* head-length repeat-times))
           ; (printf "head ~a L ~a R ~a ~a~n" head head-length repeat-times head-repeat-length)
            (define drop-tail (drop tail repeat))
           ; (printf "droptail ~a ~n" drop-tail)
            (read-data2 (drop tail repeat) (+ sumc head-repeat-length)))
           (else 
            (read-data2 (rest characters) (add1 sumc)))))))

(define (decode2 data)
  (define characters (string->list data))
  (read-data2 characters))

(string-length "XABCABCABCABCABCABCY")

(decode2 "X(8x2)(3x3)ABCY")
(decode2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")

(define test-data2
  '(("(3x3)XYZ" 9)
    ("X(8x2)(3x3)ABCY" 20)
    ("(27x12)(20x12)(13x14)(7x10)(1x12)A" 241920)
    ("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" 445)))

(for ((item test-data2))
  (check-equal? (decode2 (car item)) (second item)))

(decode2 encrypted-file)

