#lang racket

(require "advent-utils.rkt")
(require "advent7.rkt")

(define input-file "input7.txt")

(define lines (call-with-input-file input-file read-lines))

(do-part-one lines)

