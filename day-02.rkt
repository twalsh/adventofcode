#lang racket

(require rackunit)

(require "advent-utils.rkt")

(define input-ids (read-input "day-02.txt"))

(define test-ids
  '(
    "abcdef" ; contains no letters that appear exactly two or three times.
    "bababc" ; contains two a and three b, so it counts for both.
    "abbcde" ; contains two b, but no letter appears exactly three times.
    "abcccd" ; contains three c, but no letter appears exactly two times.
    "aabcdd" ; contains two a and two d, but it only counts once.
    "abcdee" ;  contains two e.
    "ababab" ))


(define (checksum ids)
  (let loop ((remaining-ids ids) (freq2 0) (freq3 0))
    (if (empty? remaining-ids)
        (* freq2 freq3)
        (let ((id (first remaining-ids)))
 
          (define letter-frequencies
            (hash-values
             (make-frequency-table (string->list id))))
         
          (loop (rest remaining-ids)
                (+ freq2
                   (if (member 2 letter-frequencies)
                       1 0))
                (+ freq3
                   (if (member 3 letter-frequencies)
                       1 0)))))))

(printf "Part One. Checksum: ~a~n" (checksum input-ids))

; Part Two
(define test-ids2 '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"))

(define (find-common-letters id1 id2)
  (for/list ((c1 id1) (c2 id2)
                      #:when (eq? c1 c2))
    c1))

(check-equal? (find-common-letters "fghij" "fguij") '(#\f #\g #\i #\j))

(define (find-id-common-letters ids common-length)
  (for*/fold
   ((common-letters ""))
   ((id1 ids) (id2 (rest ids)))
    #:break (= (string-length common-letters) common-length)
    (list->string (find-common-letters id1 id2))))

(check-equal? (find-id-common-letters test-ids2 4) "fgij")

(define id-length (string-length (first input-ids)))

(define common-letters (find-id-common-letters input-ids (- id-length 1)))

(printf "Part Two. Common letters: ~a~n" common-letters)

