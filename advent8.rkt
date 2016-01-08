#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-bytes-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define lines (call-with-input-file "input8.txt" read-lines))

(define test-strings (list "\"\"" "\"abc\"" "\"aaa\"aaa\"" "\"\x27\""))

(define (strlen bs)
  (let loop ((s (rest (bytes->list bs))) (l 0))
    (if (empty? s)
        (- l 1)
        (match s
          ((list 92 34 ...) (loop (drop s 2) (+ l 1)))
          ((list 92 120 _ _ ...) (loop (drop s 4) (+ l 1)))
          ((list 92 92 ...) (loop (drop s 2) (+ l 1)))
          ((list 34 ..1) (loop (rest s) (+ l 1)))
          (_ (loop (rest s) (+ l 1)))))))

(for ((line lines))
  (printf "~s ~s ~s~n" line (strlen line) (bytes-length line)))

(define strlens (apply + (map strlen lines)))
(define bytelens (apply + (map bytes-length lines)))

(- strlens bytelens)

