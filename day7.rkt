#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define test-ip
  '(("abba[mnop]qrst" #t) ;supports TLS(abba outside square brackets).
    ("abcd[bddb]xyyx" #f) ; does not support TLS
    ;(bddb is within square brackets, even though xyyx is outside square brackets).
    ("aaaa[qwer]tyui" #f) ;does not support TLS (aaaa is invalid; the interior characters must be different).
    ("ioxxoj[asdfgh]zxcvbn" #t))) ; supports TLS (oxxo is outside square brackets,
    ;even though it's within a larger string).

(define (tls-support? ip)
  (let loop ((ip-sequence (string->list ip)) (in-hypernet? #f) (valid-tls-ip? #f))
    (cond ((< (length ip-sequence) 4)
           valid-tls-ip?)
          (else
           (define ip-tail (rest ip-sequence))
           (case (first ip-sequence)
             ((#\[) (loop ip-tail #t valid-tls-ip?))
             ((#\]) (loop ip-tail #f valid-tls-ip?))
             (else 
              (define quad (take ip-sequence 4))
              (cond ((member #\[ quad) (loop ip-tail #t valid-tls-ip?))
                    (else
                     (define is-abba?
                       (and (eq? (first quad) (fourth quad))
                            (eq? (second quad) (third quad))
                            (not (eq? (first quad) (second quad)))))
                     (cond ((and is-abba? (not in-hypernet?)) (loop ip-tail in-hypernet? #t))
                           ((and is-abba? in-hypernet?) #f)
                           (else (loop ip-tail in-hypernet? valid-tls-ip?)))))))))))

(for ((ip test-ip))
  (check-equal? (tls-support? (first ip)) (second ip) "test-ip"))

(define puzzle-input (read-input "input7.txt"))

(define part-one (count tls-support? puzzle-input))
(displayln part-one)