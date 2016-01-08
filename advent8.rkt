#lang racket

(require rackunit)
(require "advent-utils.rkt")

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-bytes-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define lines (call-with-input-file "input8.txt" read-lines))

(define (char-length bs)
  (let loop ((s (bytes->list bs)) (len 0))
    (if (empty? s)
        len
        (if (= (car s) 34)
            (loop (rest s) len)
            (let ((char-len
                   (match s
                     ((list 92 34 _ ...) 2)
                     ((list 92 120 _ _ ...) 4)
                     ((list 92 92 _ ...) 2) 
                     (_ 1))))
              (loop (drop s char-len) (add1 len)))))))

(define total-char-length (apply + (map char-length lines)))
(define total-byte-length (apply + (map bytes-length lines)))

(define part-one (- total-byte-length total-char-length))
(printf "Day 8. Part One: ~s~n" part-one)
(check-equal? part-one 1342)

(define (encode-length bs)
  (+ 2 (for/sum ((b (bytes->list bs)))
    (if (member b '(34 92))
        2
        1))))

(define total-encode-length (apply + (map encode-length lines)))

(define part-two (- total-encode-length total-byte-length))
(printf "Day 8. Part Two: ~s~n" part-two)
(check-equal? part-two 2074)