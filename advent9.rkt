#lang racket

; Advent Day 9
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

(define (route->dist route)
  (for/sum ((i (in-range (sub1 (vector-length route)))))
    (hash-ref distances (cons (vector-ref route i)
                              (vector-ref route (add1 i))))))

(define pop (map list->vector (permutations cities)))

; Part One
(printf "Shortest route: ~s~n" (apply min (map route->dist pop)))
; Part Two
(printf "Longest route: ~s~n" (apply max (map route->dist pop)))
