#lang racket

(define (read-marker characters)
  (read-data characters))

(define (read-data characters)
  (cond ((empty? characters) '())
        (else 
         (define next (first characters))
         (case next
           ((#\()
            (read-marker characters))
           (else (cons next (read-data (rest characters))))))))

(define (decode data)
  (define characters (string->list data))
  (read-data characters))

(decode "ADVENT")
(decode "A(1x5)BC")