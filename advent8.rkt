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

(define (strlen bs)
  (let loop ((s (bytes->list bs)) (l 0))
    (if (empty? s)
        (- l 2)
        (match s
          ((list 92 34 _ ...) (loop (drop s 2) (+ l 1)))
          ((list 92 120 _ _ ...) (loop (drop s 4) (+ l 1)))
          ((list 92 92 _ ...) (loop (drop s 2) (+ l 1)))
          ((list 34 ..1) (loop (rest s) (+ l 1)))
          (_ (loop (rest s) (+ l 1)))))))

(define strlens (apply + (map strlen lines)))
(define bytelens (apply + (map bytes-length lines)))

(define part-one (- bytelens strlens))

(printf "Day 8. Part One: ~s~n" part-one)
(check-equal? part-one 1342)

