#lang racket

(provide make-edge make-graph path-sum graph-nodes string->edge)

; Make an edge from two node labels and a weight
(define (make-edge node1 node2 weight)
  (cons (list node1 node2) weight))

(define (make-graph edges)
  (make-hash edges))

; Return the list of nodes in a graph
(define (graph-nodes graph) (remove-duplicates (flatten (hash-keys graph))))

; Returns a function which takes a path and returns the sum of the weights on the path
(define (path-sum graph)
  (lambda (path)
    (for/sum ((i (in-range (sub1 (vector-length path)))))
      (let ((edge (list (vector-ref path i)
                        (vector-ref path (add1 i)))))
        (if (hash-has-key? graph edge)
            (hash-ref graph edge)
            (hash-ref graph (reverse edge)))))))

; Returns a function which takes a string extracts the
; fields numbered f1,f2,f3 (where fields are indexed from zero)
; as the two node labels and weights required to create an edge
; in the graph.
(define (string->edge f1 f2 f3)
  (lambda (s)
    (let* ((fields (string-split s " "))
           (node1 (list-ref fields f1))
           (node2 (list-ref fields f2))
           (weight (string->number (list-ref fields f3))))
      (make-edge node1 node2 weight))))
