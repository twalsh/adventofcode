#lang racket

(require rackunit)

(define (distance speed range rest-time travel-time)
  (let loop ((t 0) (flight-time 0) (state 'flying) (state-time 0))
    (if (= t travel-time)
        (* flight-time speed)
        (if (eq? state 'flying)
            (if (= state-time range)
                (loop (add1 t) flight-time 'resting 0)
                (loop (add1 t) (add1 flight-time) 'flying (add1 state-time)))
            (if (= state-time rest-time)
                (loop (add1 t) flight-time 'flying 0)
                (loop (add1 t) flight-time 'resting (add1 state-time)))))))

(define part-one (max (distance 14 10 127 2503)
                      (distance 16 11 162 2503)))

(printf "Day 14. Part One: ~s~n" part-one)
(check-equal? part-one 2640)