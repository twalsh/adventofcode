#lang racket

; Test code in advent7 using the example circuit in Part One.

(require rackunit)
(require "advent7.rkt")

(define lines '(
                "123 -> x"
                "456 -> y"
                "x AND y -> d"
                ))
;(for ((l lines))
(check-equal? (line-destruct "123 -> x") '(#f "123" #f #f "x"))
(check-equal? (line-destruct "456 -> y") '(#f "456" #f #f "y"))
(check-equal? (line-destruct "x AND y -> d") '(#f "x" "AND" "y" "d"))
(check-equal? (line-destruct "x OR y -> e") '(#f "x" "OR" "y" "e"))
(check-equal? (line-destruct "x LSHIFT 2 -> f") '(#f "x" "LSHIFT" "2" "f"))
(check-equal? (line-destruct "y RSHIFT 2 -> g") '(#f "y" "RSHIFT" "2" "g"))
(check-equal? (line-destruct "NOT x -> h") '("NOT" "x" #f #f "h"))
(check-equal? (line-destruct "NOT y -> i") '("NOT" "y" #f #f "i"))

(process "123 -> x")
(check-equal? (wire-ref "x") 123)
(process "456 -> y")
(check-equal? (wire-ref "y") 456)
(process "x AND y -> d")
(check-equal? (wire-ref "d") 72)
(process "x OR y -> e")
(check-equal? (wire-ref "e") 507)
(process "x LSHIFT 2 -> f")
(check-equal? (wire-ref "f") 492)
(process "y RSHIFT 2 -> g")
(check-equal? (wire-ref "g") 114)
(process "NOT x -> h")
(check-equal? (wire-ref "h") 65412)
(process "NOT y -> i")
(check-equal? (wire-ref "i") 65079)

wire

