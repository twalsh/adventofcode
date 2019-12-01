#lang racket

; Advent Day 9
(require "advent-graph.rkt")
(require "advent-utils.rkt")

(define lines (read-input "input9.txt"))

(define edges (map (string->edge 0 2 4) lines))
(define distances (make-graph edges))

(define pop
  (map list->vector
       (permutations (graph-nodes distances))))

(define route->dist (path-sum distances))

; Part One
(printf "Shortest route: ~s~n" (apply min (map route->dist pop)))
; Part Two
(printf "Longest route: ~s~n" (apply max (map route->dist pop)))
