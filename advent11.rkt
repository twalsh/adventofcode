#lang racket

(require rackunit)

(define input "cqjxjnds")

(define (incr-password ps)
  (list->string
   (let ((p (string->list ps)))
     (reverse (let loop ((old-p (reverse p)))
                (if (empty? old-p)
                    '()
                    (let ((c (char->integer (first old-p))))
                      (cond ((= c 122) (cons #\a (loop (rest old-p))))
                            (else (cons (integer->char (add1 c)) (rest old-p)))))))))))

; Rule 1
(define (contains-incr-str? p)
  (let loop ((rp (string->list p)))
    (if (= (length rp) 2)
        #f
        (match-let
            (((list ci cj ck) (map char->integer (take rp 3))))
          (if (and (= cj (+ 1 ci))
                   (= ck (+ 2 ci)))
              #t
              (loop (rest rp)))))))

; Rule 2
(define (contains-valid-letters? ps)
  (not (regexp-match #px"[iol]" ps)))

; Rule 3
(define (contains-letter-pairs? p)
  (and (regexp-match #px"(\\w)\\1.*(\\w)\\2" p) #t)
  )

(define (valid-password? p)
  (and (contains-incr-str? p)
       (contains-valid-letters? p)
       (contains-letter-pairs? p)))

(define test-strings '("hijklmmn" "abbceffg" "abbcegjk"))

(define (make-pw password)
  (let loop ((pw (incr-password password)))
    (if (valid-password? pw)
        pw
        (loop (incr-password pw)))))

(define part-one (make-pw input))
(printf "Day 11. Part One: ~s~n" part-one)
(check-equal? part-one "cqjxxyzz")

(define part-two (make-pw part-one))
(printf "Day 11. Part Two: ~s~n" part-two)
(check-equal? part-two "cqkaabcc")


