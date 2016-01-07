#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define lines (call-with-input-file "input8.txt" read-lines))

(define test-strings '("" "abc" "aaa\"aaa" "\x27" "aaa\\aaa"))

test-strings

(map string-length test-strings)

(define (strlen s)
  (+ (string-length s)
   (length (or (regexp-match #px"\\\\" s) '()))
   (length (or (regexp-match #px"\"" s) '()))))

(map strlen test-strings)

(define (total-chars los)
  (+ map string-length los))