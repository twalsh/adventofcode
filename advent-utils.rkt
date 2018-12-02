#lang racket

(provide read-lines read-input make-frequency-table read-table
         string->row)

(define (read-input file)
  (read-lines (open-input-file file)))

(define (read-lines in [ lines '()])
  (let ((line (read-line in 'any)))
    (if (eof-object? line)
        (reverse lines)
        (read-lines in (cons line lines)))))

(define (make-frequency-table items)
  (for/fold ((frequencies (hash)))
            ((item items))
    (hash-update frequencies item add1 0)))

(define integer-re #px"^(-?\\d+)$")

(define (string->row line)
  (for/list ((field (string-split line)))
    (if (regexp-match? integer-re field)
        (string->number field)
        (string->symbol field))))

(define (read-table lines)
  (map string->row lines))   
