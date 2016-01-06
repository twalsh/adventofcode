#lang racket

; Advent Day 9
(require "advent-graph.rkt")
(require "advent-utils.rkt")

(define lines (read-input "input9.txt"))

(define (string->edge s)
  (match-let
      (((list city1 _ city2 _ distance) (string-split s " ")))
    (make-edge city1 city2 (string->number distance))))

(define edges (map string->edge lines))

(define distances (make-graph edges))

(define cities (remove-duplicates (flatten (hash-keys distances))))

(define pop
  (map list->vector
       (permutations cities)))

(define route->dist (path-weight distances))

; Part One
(printf "Shortest route: ~s~n" (apply min (map route->dist pop)))
; Part Two
(printf "Longest route: ~s~n" (apply max (map route->dist pop)))
