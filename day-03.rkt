#lang racket

(require "advent-utils.rkt")

(define lines (read-input "input-03.txt"))

(define claim-map (make-hash))

(define (fields line)
  (match-let (
              [ (regexp #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"
                        (list _ n x y width height)) line])
    (list n x y height width)))

(map fields lines)