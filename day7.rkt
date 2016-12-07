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

(define (protocol-support? ip scan-length net-pred)
  (let loop ((ip-sequence (string->list ip)))
    (if (< (length ip-sequence) scan-length)
        #f
        (if (net-pred  (take ip-sequence scan-length))
            #t
            (loop (rest ip-sequence))))))

(define (split-ip ip)
  (define segments (regexp-split #px"[\\[\\]]" ip))

  (define networks
    (for/list ((i (in-range 0 (length segments) 2)))
      (list-ref segments i)))

  (define hypernetworks
    (for/list ((i (in-range 1 (length segments) 2)))
      (list-ref segments i)))
  (list networks hypernetworks))

(define (tls-support? ip)
  (define segments (split-ip ip)) 

  (define (is-abba? quad)
    (and (eq? (first quad) (fourth quad))
         (eq? (second quad) (third quad))
         (not (eq? (first quad) (second quad)))))
  
  (define network-abba
    (for/or ((segment (car segments)))
      (protocol-support? segment 4 is-abba?)))

  (define hypernet-abba
    (for/or ((segment (cadr segments)))
      (protocol-support? segment 4 is-abba?)))
  
  (and network-abba (not hypernet-abba)))

(for ((ip test-ip))
  (check-equal? (tls-support? (car ip)) (cadr ip) "Test valid-ip"))

(define puzzle-input (read-input "input7.txt"))

(define part-one (count tls-support? puzzle-input))
(displayln part-one)

(define (is-aba? quad)
  (and (eq? (first quad) (third quad))
       (not (eq? (first quad) (second quad)))))