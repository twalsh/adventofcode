#lang racket

(provide read-lines read-input frequency-table read-table
         string->row)

(define (read-input file)
  (read-lines (open-input-file file)))

(define (read-lines in [ lines '()])
  (let ((line (read-line in 'any)))
    (if (eof-object? line)
        (reverse lines)
        (read-lines in (cons line lines)))))

(define (frequency-table elements)
  (for/hash ((element elements))
    (values element
            (count (lambda (other-element) (eq? other-element element)) elements))))

(define integer-re #px"^(-?\\d+)$")

(define (string->row line)
  (for/list ((field (string-split line)))
    (if (regexp-match? integer-re field)
        (string->number field)
        (string->symbol field))))

(define (read-table file-name)
  (map string->row (read-input file-name)))   

