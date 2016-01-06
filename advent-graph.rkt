#lang racket

; Advent Day 12
(require "advent-utils.rkt")

(define lines (read-input "input12.txt"))

(define (make-edges lines string->edge)
  (map string->edge lines))

(define (make-graph edges)
  (let ((graph (make-hash edges)))
    (for ((k (hash-keys graph)))
      (let ((l (cons (cdr k) (car k))))
        (hash-set! graph l (hash-ref graph k))))
    graph))

(define (graph-vertices graph) (remove-duplicates (flatten (hash-keys graph))))

(define (path-value graph)
  (lambda (path)
    (for/sum ((i (in-range (sub1 (vector-length path)))))
      (hash-ref graph (cons (vector-ref path i)
                            (vector-ref path (add1 i)))))))

(define (graph-paths graph)
  (map list->vector
       (permutations (graph-vertices graph))))

(define (path-values graph)
  (map (path-value graph) graph-paths))

; Part One
(printf "Shortest route: ~s~n" (apply min (path-values graph)))
; Part Two
(printf "Longest route: ~s~n" (apply max (path-value graphs)))
