#!/usr/bin/env racket
#lang racket
(require "advent-utils.rkt") 

(define changes (map string->number (read-input "input-01.txt")))

; Part One
(define final-frequency (apply + changes))
(printf "Final frequency ~a~n" final-frequency)

; Part Two
(define first-repeated-frequency
  (let loop ((frequency 0) (remaining-changes changes) (frequencies (set)))
    (cond ((empty? remaining-changes)
           (loop frequency changes frequencies))
          (else
           (define change (first remaining-changes))
           (define next-frequency (+ frequency change))
           (if (set-member? frequencies next-frequency)
               next-frequency
               (loop next-frequency
                     (rest remaining-changes)
                     (set-add frequencies next-frequency)))))))

(printf "First repeated Frequency ~a~n" first-repeated-frequency)






