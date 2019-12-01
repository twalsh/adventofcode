#lang racket

(require rackunit)
(require "advent-utils.rkt")

(struct reindeer (name speed range rest-time) #:transparent)

; Returns function which takes a reindeer struct and returns
; the amount of time the reindeer has spent flying at each second from
; 1 to travel-time
(define (flight-path travel-time)
  (lambda (d)
    (let ((range (reindeer-range d))
          (rest-time (reindeer-rest-time d)))
      (map (lambda (p)
             (* p (reindeer-speed d)))
           (let loop ((fpath '())
                      (t 0)
                      (ft 0)
                      (state 'flying)
                      (state-time 0))
             (if (= t travel-time)
                 '()
                 (if (eq? state 'flying)
                     (if (= state-time range)
                         (cons ft (loop fpath (add1 t) ft 'resting 1))
                         (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying (add1 state-time))))
                     (if (= state-time rest-time)
                         (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying 1))
                         (cons ft (loop fpath (add1 t) ft 'resting (add1 state-time)))))))))))

; Read reindeer descriptions
(define lines (read-input "input14.txt"))

; Create a reindeer struct by reading an input line
(define (line->reindeer line)
  (match-let (((list name _ _ speed _ _ range _ ..6 rest-time _)
               (string-split line " ")))
    (reindeer name
              (string->number speed)
              (string->number range)
              (string->number rest-time))))

; Create list of reindeer from input
(define team (map line->reindeer lines))

; Generate flight paths for reindeer
(define paths (map (flight-path 2503) team))

(define part-one (apply max (map last paths)))
(printf "Day 14. Part One: ~s~n" part-one)
(check-equal? part-one 2640)

; Generate a list of winners at each point along the flight path.
; The list elements are the reindeer which have scored one point at
; that time point. The reindeer are identified by the index of their
; position in the reindeer list.
(define winner-list
  (let loop ((rp paths) (winners '()))
    (if (empty? (car rp))
        winners
        ; Make a list of distances at this time point by taking the
        ; first element of each flight path. Extract the
        ; maximum distance.
        (let* ((distances (map car rp))
               (max-dist (apply max distances)))
          ; Generate the list of reindeer that get a point at this time
          ; point, which are those with the maximum distance.
          (define winners-this-point
            (filter (lambda (i)
                      (= (list-ref distances i) max-dist))
                    (range (length team))))
          (loop (map rest rp) (cons winners-this-point winners))))))

; Count the number of time points in wl at which each reindeer scores a point
; and takes the maximum count as the winning score
(define part-two (apply max (for/list ((i (in-range (length team))))
                              (count (lambda (l) (member i l)) winner-list))))

(printf "Day 14. Part Two: ~s~n" part-two)
(check-equal? part-two 1102)