#lang racket

(provide read-lines read-input)

(define (read-input file)
  (read-lines (open-input-file file)))

(define (read-lines in)
  (let loop ((lines '()))
    (let ((line (read-line in)))
      (if (eof-object? line)
        (reverse lines)
        (loop (cons line lines))))))