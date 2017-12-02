#lang racket
(require "advent-utils.rkt")

(define test-lines
  '("5 1 9 5"
    "7 5 3"
    "2 4 6 8"))

(define test-sheet (read-table test-lines))

(define (checksum sheet)
  (for/fold ([sum 0])
            ([row sheet])
    (define diff (- (apply max row) (apply min row)))
  (values (+ sum diff))))

(checksum test-sheet)

(define puzzle-data (read-table (read-input "day02.in")))

(checksum puzzle-data)
