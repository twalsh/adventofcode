#lang racket
(require gregor)
(require rackunit)

(require "advent-utils.rkt")

(define line-sorter (lambda (a b) (string<? (substring a 1 17) (substring b 1 17))))


(define test-lines (read-input "test-04.txt"))
(define sorted-test-lines (sort test-lines line-sorter))

(struct log-entry (date time log id type) #:transparent)


(define (lines->entries lines)
 
  (let loop ((remaining-lines lines) (old-guard '()))
    
    (if (empty? remaining-lines)
        '()
        (let ((line (first remaining-lines)))
          (printf "~a~n" line)
          (define new-entry
            (match-let (
                        [ (regexp #px"\\[(\\S+) (\\d{2}:\\d{2})\\] (.*)"
                                  (list _ date-str time-str log-str))
                         
      
                          line])
              (define-values (new-guard type)
                (match log-str
                  [ (regexp #px"Guard #(\\d+)" (list _ id)) (values id 'start)]
                  [ "wakes up" (values old-guard 'awake) ]
                  [ "falls asleep" (values old-guard 'asleep) ]))
              
              (log-entry (iso8601->date date-str) (iso8601->time time-str) log-str new-guard type)))
          
          (cons new-entry (loop (rest remaining-lines) (log-entry-id new-entry)))))))



(struct record (date time id minutes) #:transparent #:mutable)
(define entries (lines->entries test-lines))

(define (adjust-datetime idate itime)
  (if (= (->hours itime) 23)
      (values (+days idate 1) (time 0))
      (values idate itime)))


(define-values (test-date test-hour) (values (iso8601->date "1518-11-01")
                                             (iso8601->time "00:55")))

(let-values (((adate atime) (adjust-datetime test-date test-hour)))
  (printf "~a ~a~n" adate atime))

(define-values (test-date2 test-hour2) (values (iso8601->date "1518-11-01")
                                               (iso8601->time "23:58")))

(let-values (((adate atime) (adjust-datetime test-date2 test-hour2)))
  (printf "~a ~a~n" adate atime))

(define (start entries records)
  (if (empty? entries)
      records
      (let ((e (first entries)))
        (define-values (start-date start-time)
          (adjust-datetime (log-entry-date e) (log-entry-time e)))
        (define timesheet (make-vector 60 #\.))
        (sleep (rest entries) start-date start-time timesheet records))))

(define (sleep entries start-date start-time timesheet records)
  (define e (first entries))
  (define-values (sleep-date sleep-time)
    (adjust-datetime (log-entry-date e) (log-entry-time e)))
  (wake (rest entries) sleep-date sleep-time timesheet records))

(define (wake entries sleep-date sleep-time timesheet reocrds)
  (define e (first entries))
  (define-values (wake-date wake-time)
    (adjust-datetime (log-entry-date e) (log-entry-time e)))
  (for/vector ((i (in-range sleep-time (add1 wake-time))))
      (vector-set! timesheet i #\#))
  (define new-record (record wake-date (log-entry-id e) timesheet)
  
  
;    (if (log-entry-time e)
;    (define date (log-entry-date e))
;  (define time (log-entry-time e))
;(let start ((remaining-entries entries))
;  (if (empty? remaining-entries)
;      '()
;      (let ((e (first remaining-entries)))
;          (cond ((eq? (log-entry-type e) 'start)
;                 (loop (rest remaining-entries)
;                       (log-entry-date e)
;                       (log-entry-id e)
;                       (log-entry-
;                                           (make-vector 60 #\.))))
;                   (loop (rest remaining-entries) (cons new-record records))))
;                ((eq? (log-entry-type e) 'awake)
;                 (define r (first records))
;                 (vector-set! (record-minutes r) (log-entry-time e)
;                 (loop (rest remaining-entries) records)))))))
;        
           
