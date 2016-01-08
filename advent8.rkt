#lang racket

(require rackunit)
(require "advent-utils.rkt")

;(define (read-lines in)
;  (let loop ((lines '()))
;    (let ((line (read-bytes-line in)))
;      (if (eof-object? line)
;        (reverse lines)
;        (loop (cons line lines))))))

(define lines (call-with-input-file "input8.txt" read-lines))

(define test-strings (list "\"\"" "\"abc\"" "\"aaa\"aaa\"" "\"\x27\""))

test-strings

(map string-length test-strings)

(define (strlen s)
  (- (string-length s) 2))
   ;(length (or (regexp-match #px"\\" s) '()))
   ;(length (or (regexp-match #px"\"" s) '()))))

(define (strlen2 s)
  (+ (string-length s)
     (length (or (regexp-match* #px"\\\\" s) empty))
     (- (length (or (regexp-match* #px"\"" s) empty)) 2)))

(map strlen test-strings)
(map strlen2 test-strings)

(define (total-chars-mem ts)
  (apply + (map strlen ts)))

(define (total-chars-str ts) 
  (apply + (map strlen2 ts)))

(printf "total-chars-str: ~s~n" (total-chars-str lines))
(printf "total-chars-mem: ~s~n" (total-chars-mem lines))

(define (diff s) (- (total-chars-str s) (total-chars-mem s)))

(printf "~s~n" (diff lines))