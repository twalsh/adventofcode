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
        '()
        (let ((quad (take ip-sequence scan-length)))
          (if (net-pred quad) 
              (cons quad (loop (rest ip-sequence)))
              (loop (rest ip-sequence)))))))
  
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
      (not (empty?
            (protocol-support? segment 4 is-abba?)))))

  (define hypernet-abba
    (for/or ((segment (cadr segments)))
      (not (empty?
            (protocol-support? segment 4 is-abba?)))))
  
  (and network-abba (not hypernet-abba)))

(for ((ip test-ip))
  (check-equal? (tls-support? (car ip)) (cadr ip) "Test valid-ip"))

(define puzzle-input (read-input "input7.txt"))

(define part-one (count tls-support? puzzle-input))
(displayln part-one)

(define (ssl-support? ip)
  (define segments (split-ip ip))
  
  (define (is-aba? quad)
    (and (eq? (first quad) (third quad))
         (not (eq? (first quad) (second quad)))))
  
  (define net-segments
    (append*
     (for/list ((segment (car segments)))
       (protocol-support? segment 3 is-aba?))))
 
  (define hypernet-segments
    (append*
     (for/list ((segment (cadr segments)))
       (protocol-support? segment 3 is-aba?))))
    
  (filter
   (lambda (segment)
     (define bab-segment (list (second segment) (first segment) (second segment)))
     (member bab-segment hypernet-segments))
   net-segments))

(define test-data-2 '(("aba[bab]xyz" #t) 
                      ("xyx[xyx]xyx" #f) 
                      ("aaa[kek]eke" #t)
                      ("zazbz[bzb]cdb" #t)))

(for ((datum test-data-2))
  (check-equal? (not (empty? (ssl-support? (car datum)))) (cadr datum) "Part 2 test"))

(define segs (ssl-support? (caar test-data-2)))
(define part-2
  (count
   (lambda (ip) (not (empty? (ssl-support? ip))))
   puzzle-input))

(displayln part-2)
