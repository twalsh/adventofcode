#lang racket

(require rackunit)
(require "advent-utils.rkt")

; Fingerprint of the aunt we're looking for.
(define fingerprint #hash(("children" . 3)
                          ("cats" . 7)
                          ("samoyeds" . 2)
                          ("pomeranians" . 3)
                          ("akitas" . 0)
                          ("vizslas" . 0)
                          ("goldfish" . 5)
                          ("trees" . 3)
                          ("cars" . 2)
                          ("perfumes" . 1)))

(define prop-name (hash-keys fingerprint))

; Aunt number + fingerprint hash
(struct aunt (number fingerprint))

(define lines (read-input "input16.txt"))

; Given string of properties, generate fingerprint hash
(define (string->fingerprint line)
  (make-hash
   (map (lambda (p)
          (cons (car p) (string->number (cadr p))))
        (regexp-match* #px"(\\w+): (\\d+)" line #:match-select cdr))))

; Convert line of input to aunt struct
(define (line->aunt line)
  (match-let
      ; Split line into aunt index and properties
      (((regexp #px"Sue (\\d+):(.*)" (list _ n properties)) line))
    (define property-pairs
      (string->fingerprint properties))
    (aunt (string->number n) property-pairs)))

(define aunts (map line->aunt lines))

; Compare properties of fingerprint to aunt's
(define (compare cmpfunc)
  (lambda (aunt)
    (for/and ((p prop-name))
      (let ((prop-value (hash-ref fingerprint p))
            (aunt-value (hash-ref (aunt-fingerprint aunt) p '())))
        (if (eq? aunt-value '())
            ; aunt property not defined so don't use for comparison
            #t
            ; Else compare the aunt's value to the fingerprint's
            (let ((cmp (cmpfunc p)))
              (cmp prop-value aunt-value)))))))

; Find the correct aunt using the supplied function to work
; out how to compare each property in the fingerprint.
(define (find-aunt cmpfunc)
   (aunt-number (first (filter (compare cmpfunc) aunts))))

; Get answer for part one. Comparator function is = for all properties.
(define part-one
  (find-aunt (lambda (p) =)))
(check-equal? part-one 373)
(printf "Day 16. Part One: ~s~n" part-one)

; Generate comparator for part two.
(define (part-two-cmpfunc prop-name)
  (match prop-name
    ((or "cats" "trees") <)
    ((or "pomeranians" "goldfish") >)
    (_ =)))

(define part-two
  (find-aunt part-two-cmpfunc))

(printf "Day 16. Part Two: ~s~n" part-two)
(check-equal? part-two 260)