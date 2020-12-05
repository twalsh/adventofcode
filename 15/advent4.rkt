#lang racket

(require openssl/md5)

(define key "bgvyzdsv")

(define (number->md5 i)
    (md5 (open-input-string
             (string-append key 
                            (number->string i)))))

(define (zero-prefix n [start 0])
  (define prefix (make-string n #\0))
  
  (let loop ((i start))
    (let ((hash (number->md5 i))) 
      (if (string=? (substring hash 0 n) prefix)
        (values i hash)
        (loop (+ i 1))))))
           
(define-values (part-one-n part-one-hash)
               (zero-prefix 5))
(printf "Part One: ~a~n" part-one-n)

(define-values (part-two-n part-two-hash)
               (zero-prefix 6 part-one-n))
(printf "Part Two ~a~n" part-two-n)

