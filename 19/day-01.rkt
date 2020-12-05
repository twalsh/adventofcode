#lang racket
(require rackunit)
(require "../advent-utils.rkt")

(define (mass->fuel mass)
  (- (floor (/ mass 3)) 2))

(define (mass->fuel2 mass)
  (define (fuel->fuel fuel-mass)
    (define add-fuel (mass->fuel fuel-mass))
    
    (if (<= add-fuel 0)
        fuel-mass
        (+ fuel-mass (fuel->fuel add-fuel))))
  (fuel->fuel (mass->fuel mass)))

(check-eq? (mass->fuel 12) 2)
(check-eq? (mass->fuel 14) 2)
(check-eq? (mass->fuel 1969) 654)
(check-eq? (mass->fuel 100756) 33583)

(define module-masses (map string->number (read-input "input-01.txt")))

(define fuel-masses (map mass->fuel module-masses))

(define total-fuel-mass (apply + fuel-masses))

(printf "Day 01, part 01: ~s~n" total-fuel-mass)

(check-eq? (mass->fuel2 14) 2)
(check-eq? (mass->fuel2 1969) 966)
(check-eq? (mass->fuel2 100756) 50346)

(define fuel-masses2 (map mass->fuel2 module-masses))

(define total-fuel-mass2 (apply + fuel-masses2))

(printf "Day 01, part 02: ~s~n" total-fuel-mass2)
