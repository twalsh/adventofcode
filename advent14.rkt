#lang racket

(require rackunit)
(require "advent-utils.rkt")

(struct deer (name speed range rest-time) #:transparent)

; Returns function which takes a deer struct and returns
; the amount of time the deer has spent flying at each second from
; 1 to travelt-time
(define (flight-path travel-time)
  (lambda (d)
    (let ((range (deer-range d)) (rest-time (deer-rest-time d)))
      (let loop ((fpath '())
                 (t 0)
                 (ft 0)
                 (state 'flying)
                 (state-time 0))
        (if (= t travel-time)
            fpath            
            (if (eq? state 'flying)
                (if (= state-time range)
                    (cons ft (loop fpath (add1 t) ft 'resting 1))
                    (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying (add1 state-time))))
                (if (= state-time rest-time)
                    (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying 1))
                    (cons ft (loop fpath (add1 t) ft 'resting (add1 state-time))))))))))

; Calculate the distance a deer has travelled at each time point
; along the given flight path
(define (distances d path)
  (for/list ((p path))
    (* (deer-speed d) p)))

; Read deer descriptions
(define lines (read-input "input14.txt"))

; Create a deer struct by reading an input line
(define (line->deer line)
  (match-let (((list name _ _ speed _ _ range _ ..6 rest-time _)
               (string-split line " ")))
    (deer name
          (string->number speed)
          (string->number range)
          (string->number rest-time))))

; Create list of deer from input
(define dl (map line->deer lines))

; Generate flight paths for deer
(define paths (map (lambda (d)
                     (distances d ((flight-path 2503) d)))
                   dl))

(define part-one (apply max (map last paths)))
(printf "Day 14. Part One: ~s~n" part-one)
(check-equal? part-one 2640)

; Generate a list of winners at each point along the flight path.
; The list elements are the deer which have scored one point at
; that time point. The deer are identified by the index of their
; position in the deer list.
(define wl
  (let loop ((rp paths) (winners '()))
    (if (empty? (car rp))
        winners
        ; Make a list of distances at this time point by taking the
        ; first element of each flight path. Extract the
        ; maximum distance.
        (let* ((distances (map car rp))
               (max-dist (apply max distances)))
          ; Generate the list of deer that get a point at this time
          ; point, which are those with the maximum distance.
          (define winners-this-point
            (filter (lambda (i)
                      (= (list-ref distances i) max-dist))
                    (range (length dl))))
          (loop (map rest rp) (cons winners-this-point winners))))))

(define part-two (apply max (for/list ((i (in-range (length dl))))
                              (count (lambda (l) (member i l)) wl))))

(printf "Day 14. Part Two: ~s~n" part-two)
(check-equal? part-two 1102)