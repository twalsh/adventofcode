#lang racket

(require rackunit)
(require srfi/1)

(require "advent-utils.rkt")

(define test-lines
  '("aaaaa-bbb-z-y-x-123[abxyz]" 
    ; is a real room because the most common letters are a (5), b (3), and then a tie between x, y,
    ; and z, which are listed alphabetically.
    "a-b-c-d-e-f-g-h-987[abcde]" 
    ; is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
    "not-a-real-room-404[oarel]"  ; is a real room.
    "totally-real-room-200[decoy]" )) ; is not.

(define (fields line [sep " "])
  (string-split line sep))

(define (frequency-table elements)
  (for/hash ((element elements))
    (values element
            (count (lambda (other-element) (eq? other-element element)) elements))))

(define (frequency-table-reverse table)
  (define counts (hash-values table))
  counts)

(define (real-room? room)
  (define line-fields (fields room "-"))
  
  (define-values (encrypted-name sector-id-checksum)
    (split-at-right line-fields 1))

  (define-values (sector-id checksum)
    (match-let (((list _ sector-id checksum)
                 (regexp-match #px"(\\d+)\\[(\\w+)\\]" (car sector-id-checksum))))
      (values sector-id checksum)))

  (define letters (append-map string->list encrypted-name))
  (define letter-frequencies (hash->list (frequency-table letters)))
  
  ; Collect and sort counts
  (define frequencies (delete-duplicates (sort (map cdr letter-frequencies) >=)))

  (define (letter-sort a b) (char<=? (car a) (car b)))
  
  (define sorted-by-count
     (for/list ((freq frequencies))
       (define letters-with-count 
         (filter (lambda (letter-count) (= (cdr letter-count) freq)) letter-frequencies))
       (sort letters-with-count letter-sort)))

  ;(displayln sorted-by-count)

  (define sorted-letter-list
     (flatten
      (for/list ((count-list sorted-by-count))
        (map car count-list))))

  (define sorted-letters
    (list->string
     (take sorted-letter-list 5)))

  ;(displayln sorted-letters)
     
  (if (string=? sorted-letters checksum)
      (string->number sector-id)
      0))

(define sum-sector-ids
  (for/sum ((line test-lines))
    (real-room? line)))

(check-equal? sum-sector-ids 1514 "Test sum = 1514")

(define puzzle-data (read-input "input4.txt"))

(define puzzle-sector-ids (map real-room? puzzle-data))
(define puzzle-sum (apply + puzzle-sector-ids))
(displayln puzzle-sum)