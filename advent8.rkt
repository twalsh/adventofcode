#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define lines (call-with-input-file "input8.txt" read-lines))

(define test-strings '("" "abc" "aaa\"aaa" "\x27" "aaa\\aaa"))

test-strings

(map string-length test-strings)

(map (lambda (s) (regexp-match #px"\\\\" s)) test-strings)

(map (lambda (s) (regexp-match #px"\"" s)) test-strings)