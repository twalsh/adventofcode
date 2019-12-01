#lang racket

; Advent Day 13
(require "advent-graph.rkt")
(require "advent-utils.rkt")

(define lines (read-input "input13.txt"))

(define (string->edge f1 f2 f3)
  (lambda (s)
    (let* ((fields (string-split s " "))
           (node1 (list-ref fields f1))
           (node2 (string-trim (list-ref fields f2) "."))
           (weight
            (if (string=? (third fields) "gain")
                (string->number (list-ref fields f3))
                (- (string->number (list-ref fields f3))))))
      (make-edge node1 node2 weight))))

(define edges (map (string->edge 0 10 3) lines))

(define graph (make-graph edges))

(define (paths graph)
  (map (lambda (l) (flatten (cons (take-right l 1) l)))
       (permutations (graph-nodes graph))))

(define (path-sum-bidirectional graph)
  (lambda (path)
    (+ ((path-sum graph) (list->vector path))
       ((path-sum graph) (list->vector (reverse path)))
    )))

; Part One
(printf "Part One. Total change in happiness: ~s~n" (apply max (map (path-sum-bidirectional graph) (paths graph))))

(for ((node (graph-nodes graph)))
  (hash-set! graph (list "Me" node) 0))
; Part Two
(printf "Part Two. Total change in happiness: ~s~n" (apply max (map (path-sum-bidirectional graph) (paths graph))))