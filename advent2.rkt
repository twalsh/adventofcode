#lang racket

(define (get-ribbon dimensions)
  ;(+ (* 2 (apply + (take (sort dimensions <) 2)))
  (+ (* 2 (apply + (take dimensions 2)))
     (apply * dimensions)))

(define (get-paper dimensions)
  (match-let (((list l w h) dimensions))
    (+ (* 2
          (+
           (* l w)
           (* w h)
           (* l h)))
       ; Extra paper
       (apply * (take dimensions 2)))))

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))


(define dimensions 
  (let ((lines (call-with-input-file "input2.txt" read-lines)))
    (for/list ((line lines))
              (define fields (string-split line "x"))
              (define dimensions (map string->number fields))
              (sort dimensions <))))

(printf "Total square feet of wrapping paper: ~s~n"
        (apply + (map get-paper dimensions)))
(printf "Total feet of ribbon: ~s~n"
        (apply + (map get-ribbon dimensions)))

