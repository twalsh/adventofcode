#lang racket
(require "advent-utils.rkt")

(define test-lines
  '("5 1 9 5"
    "7 5 3"
    "2 4 6 8"))

(define test-sheet (read-table test-lines))

(define (checksum sheet)
  (for/sum ([row sheet])
    (- (apply max row) (apply min row))))

(checksum test-sheet)

(define puzzle-data (read-table (read-input "day02.in")))

(checksum puzzle-data)
