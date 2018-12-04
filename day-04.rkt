#lang racket
(require gregor)
(require rackunit)

(require "advent-utils.rkt")

(define line-sorter (lambda (a b) (string<? (substring a 1 17) (substring b 1 17))))


(define test-lines (read-input "test-04.txt"))
(define sorted-test-lines (sort test-lines line-sorter))

;(define lines (read-input "input-04.txt"))
;(define sorted-lines (sort lines line-sorter))

;(substring (first sorted-lines) 1 17)
;(iso8601->time (substring (first sorted-lines) 12 17))

(struct record (date-str time-str log-str id type) #:transparent)

  

(define (lines->records lines)
  (let loop ((remaining-lines lines) (old-guard '()))
    (if (empty? remaining-lines)
        '()
        (let ((line (first remaining-lines)))
          (define new-record
            (match-let (
                        [ (regexp #px"\\[(\\S+) (\\d{2}:\\d{2})\\] (.*)"
                                  (list _ date-str time-str log-str))
                          ;\\s+(\\d\\d:\\d\\d)] (.*)"
                          ;        (list _ date time log line))
                          line])
              (define-values (new-guard type)
                (match log-str
                  [ (regexp #px"Guard #(\\d+)" (list _ id)) (values id 'start)]
                  [ "wakes up" (values old-guard 'awake) ]
                  [ "falls asleep" (values old-guard 'asleep) ]))
              
              (record date-str time-str log-str new-guard type)))
          
          (cons new-record (loop (rest remaining-lines) (record-id new-record)))))))

(lines->records test-lines)
;(define (line->record line)

;            [ (regexp #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"
;                     (list _ n x y width height)) line])
;(claim (string->number n)
;      (string->number x)
;     (string->number y)
;;    (string->number height)
;  (string->number width)))) 