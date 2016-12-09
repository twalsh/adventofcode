#lang racket

(provide read-lines read-input frequency-table rows->columns read-table)

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

(define (rows->columns rows)
  (define width (length (first rows)))
  (for/list ((i (in-range width)))
    (for/list ((row rows))
      (list-ref row i))))

(define integer-re #px"(\\d+)")

(define (read-table file-name)
  (for/list ((line (read-input file-name)))
    (for/list ((field (string-split line)))
      (if (regexp-match? integer-re field)
          (string->number field)
          field))))

