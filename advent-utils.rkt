#lang racket

(provide read-lines read-input frequency-table)

(define (read-input file)
  (read-lines (open-input-file file)))

(define (read-lines in [ lines '()])
  (let ((line (read-line in)))
    (if (eof-object? line)
        (reverse lines)
        (read-lines in (cons line lines)))))

(define (frequency-table elements)
  (for/hash ((element elements))
    (values element
            (count (lambda (other-element) (eq? other-element element)) elements))))
