#lang racket

(define (get-ribbon dimensions)
  (+ (* 2 (apply + (take (sort dimensions <) 2)))
     (apply * dimensions)))

(define (get-paper dimensions)
  (match-let (((list l w h) dimensions))
    (+ (* 2
          (+
           (* l w)
           (* w h)
           (* h l)))
       ; Extra paper
       (apply * (take (sort dimensions <) 2)))))

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))

(define dimensions (map 
                     (lambda (s) (map string->number (string-split s "x"))) 
                     (call-with-input-file "input2.txt" read-lines)))

(for/sum ((p (map get-paper dimensions))) p)
(for/sum ((p (map get-ribbon dimensions))) p)

