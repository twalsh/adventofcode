#lang racket

(require "advent-utils.rkt")

(define test-phrases '("aa bb cc dd ee"
                       "aa bb cc dd aa"
                       "aa bb cc dd aaa"))

(define (is-valid? phrase compare-words)
  (define words (string-split phrase))
  (let loop ((probe (first words))
             (target (rest words)))
    (if (empty? target)
        #t
        (if (compare-words probe target)
            #f
            (loop (first target) (rest target))))))

(for ((phrase test-phrases))
  (printf "~a ~a~n" phrase (is-valid? phrase member)))

(define puzzle-phrases (read-input "day04.in"))
(count (lambda (p) (is-valid? p member)) puzzle-phrases)

(define (is-anagram? word1 word-list)
  (define letters-1 (sort (string->list word1) char<?))
  (for/or ((word2 word-list))
    (define letters-2 (sort (string->list word2) char<?))
    (equal? letters-1 letters-2)))

(count (lambda (p) (is-valid? p is-anagram?)) puzzle-phrases)