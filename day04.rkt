#lang racket

(require "advent-utils.rkt")

(define test-phrases '("aa bb cc dd ee"
                       "aa bb cc dd aa"
                       "aa bb cc dd aaa"))

(define (make-validation-function compare-words)
  (lambda (phrase)
    (define words (string-split phrase))
    (let loop ((probe (first words))
               (target (rest words)))
      (if (empty? target)
          #t
          (if (compare-words probe target)
              #f
              (loop (first target) (rest target)))))))

(define no-duplicate-words? (make-validation-function member))

(for ((phrase test-phrases))
  (printf "~a ~a~n" phrase (no-duplicate-words? phrase)))

(define puzzle-phrases (read-input "day04.in"))
(count no-duplicate-words? puzzle-phrases)

(define (is-anagram? word1 word-list)
  (define letters-1 (sort (string->list word1) char<?))
  (for/or ((word2 word-list))
    (define letters-2 (sort (string->list word2) char<?))
    (equal? letters-1 letters-2)))

(count (make-validation-function is-anagram?) puzzle-phrases)