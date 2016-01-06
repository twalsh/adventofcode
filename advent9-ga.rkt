#lang racket

(require "advent-utils.rkt")

(define lines (read-input "input9.txt"))

(define (string->distance s)
  (match-let
      (((list city1 _ city2 _ distance) (string-split s " ")))
    (cons (cons city1 city2) distance)))

(define distances (make-hash (map
                              (lambda (p) (cons (car p) (string->number (cdr p))))
                              (map string->distance lines))))

(for ((k (hash-keys distances)))
  (let ((l (cons (cdr k) (car k))))
    (hash-set! distances l (hash-ref distances k))))

(define cities (remove-duplicates (flatten (hash-keys distances))))

(define (make-chrom cities)
  (list->vector (shuffle cities)))

(define (chrom->dist chrom)
  (for/sum ((i (in-range (sub1 (vector-length chrom)))))
    (hash-ref distances (cons (vector-ref chrom i)
                              (vector-ref chrom (add1 i))))))

(define (mutate chrom)
  (let ((i (random (vector-length chrom)))
        (j (random (vector-length chrom))))
    (let ((c1 (vector-ref chrom i))
          (c2 (vector-ref chrom j)))
      (vector-set! chrom i c2)
      (vector-set! chrom j c1))
    chrom))

(define pop (map list->vector (permutations cities)))

(apply min (map chrom->dist pop))
(apply max (map chrom->dist pop))
