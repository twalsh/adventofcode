#lang racket

(require "advent-utils.rkt")

(provide make-edge make-graph path-weight graph-nodes string->edge)

(define (make-edge node1 node2 weight)
    (cons (list node1 node2) weight))

(define (make-graph edges)
  (make-hash edges))

(define (graph-nodes graph) (remove-duplicates (flatten (hash-keys graph))))

(define (path-weight graph)
  (lambda (path)
  (for/sum ((i (in-range (sub1 (vector-length path)))))
    (let ((edge (list (vector-ref path i)
                      (vector-ref path (add1 i)))))
      (if (hash-has-key? graph edge)
          (hash-ref graph edge)
          (hash-ref graph (reverse edge)))))))

(define (graph-paths graph)
  (map list->vector
       (permutations (graph-nodes graph))))

(define (string->edge f)
  (lambda (s)
    (let* ((fields (string-split s " "))
           (node1 (list-ref fields (first f)))
           (node2 (list-ref fields (second f)))
           (weight (string->number (list-ref fields (third f)))))
      (make-edge node1 node2 weight))))

