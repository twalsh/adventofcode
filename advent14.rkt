#lang racket

(require rackunit)

(struct deer (speed range rest-time))
; Flight point - total flying time, clock time, state (flying/resting)
(struct fpoint (ftime time state))

(define (flight-path travel-time)
  (lambda (d)
    (let ((range (deer-range d)) (rest-time (deer-rest-time d)))
      (let loop ((fpath '())
                 (t 0)
                 (ft 0)
                 (state 'flying)
                 (state-time 0))
        (printf "~s ~s ~s ~s~n" t ft state-time state)
        (if (= t travel-time)
            fpath
           
            (if (eq? state 'flying)
                (if (= state-time range)
                    (cons ft (loop fpath (add1 t) ft 'resting 1))
                    (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying (add1 state-time))))
                (if (= state-time rest-time)
                    (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying 1))
                    (cons ft (loop fpath (add1 t) ft 'resting (add1 state-time))))))))))

(define (distances d path)
  (for/list ((p path))
    (* (deer-speed d) p)))

(define (distance travel-time)
  (lambda (d)
    (* (deer-speed d)
       (last ((flight-path travel-time) d)))))

(define dl (list (deer 14 10 127)
                 (deer 16 11 162)))

;((distance 1) (car dl))
;((distance 10) (car dl))
;((distance 1000) (car dl))

(define part-one (apply max (map (distance 2503) dl)))
(printf "Day 14. Part One: ~s~n" part-one)
;(check-equal? part-one 2640)

(define paths (map (lambda (d)
                     (distances d ((flight-path 2503) d))) dl))

(define score1 0)
(define score2 0)

(define p1 (car paths))
(define p2 (cadr paths))

(for/fold ((i 1)
           (score1 0)
           (score2 0))
          ((f1 p1) (f2 p2))
  (printf "COMP ~s C ~s ~s : D ~s ~s~n" i f1 score1 f2 score2)
  (cond ((> f1 f2)
         (values (add1 i) (add1 score1) score2))
        ((< f1 f2)
         (values (add1 i) score1 (add1 score2)))
        (else
         (values (add1 i) (add1 score1) (add1 score2)))))

score1
score2