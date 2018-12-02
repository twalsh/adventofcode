#lang racket

(require "advent-utils.rkt")

(define ids (read-input "day-02.txt"))

(first ids)
(make-frequency-table (string->list (first ids)))