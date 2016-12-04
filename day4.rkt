#lang racket

(define test-lines
  '("aaaaa-bbb-z-y-x-123[abxyz]" 
    ; is a real room because the most common letters are a (5), b (3), and then a tie between x, y,
    ; and z, which are listed alphabetically.
    "a-b-c-d-e-f-g-h-987[abcde]" 
    ; is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
    "not-a-real-room-404[oarel]"  ; is a real room.
    "totally-real-room-200[decoy]" )) ; is not.

(define line (caar test-lines))

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

  (define (letter-sort a b)
    (and 
     (>= (cdr a) (cdr b))
     1
     ;(char<=? (car a) (car b))
     ))

  (printf "~a ~n" letter-frequencies)
  
  (define sorted-letters
    (list->string
     (take
      (map car
           (sort letter-frequencies letter-sort))
      5)))
     
  (printf "~a ~a~n" sorted-letters checksum)
  (if (string=? sorted-letters checksum)
      (string->number sector-id)
      0))

(real-room? (first test-lines))

