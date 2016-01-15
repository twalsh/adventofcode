#lang racket

(require rackunit)

(struct deer (speed range rest-time))
; Flight point - total flying time, clock time, state (flying/resting)
(struct fpoint (ftime time state))

(define (make-fpoint fpoint)
  (fpoint ftime time state))
  
(define (flight-path travel-time)
  (lambda (d)
    (let ((range (deer-range d)) (rest-time (deer-rest-time d)))
      (let loop ((fpath '())
                 (fpoint 0 0 'flying) (state-time 0))
        (if (= t travel-time)
            fpath
            (let ((fp (fpoint ft t state)))
              (if (eq? state 'flying)
                  (if (= state-time range)
                      (cons fp (loop fpath (add1 t) ft 'resting 0))
                      (cons (add1 ft) (loop fpath (add1 t) (add1 ft) 'flying (add1 state-time))))
                  (if (= state-time rest-time)
                      (cons fp (loop fpath (add1 t) ft 'flying 0))
                      (cons fp (loop fpath (add1 t) ft 'resting (add1 state-time)))))))))))
  
(define (distances d path)
  (for/list ((p path))
    (* (deer-speed d) p)))

(define (distance travel-time)
  (lambda (d)
    (* (deer-speed d)
       (last ((flight-path travel-time) d)))))

(define dl (list (deer 14 10 127)
                 (deer 16 11 162)))

((distance 1) (car dl))
((distance 10) (car dl))
((distance 1000) (car dl))

(define part-one (apply max (map (distance 2503) dl)))

(printf "Day 14. Part One: ~s~n" part-one)
(check-equal? part-one 2640)

(define paths (map (lambda (d)
                     (distances d ((flight-path 1000) d))) dl))

(printf "~s~n" paths)
(map length paths)

(define score1 0)
(define score2 0)

(define p1 (car paths))
(define p2 (cadr paths))

p1
p2

(for ((i (in-range (length p1)))) 
  (let ((f1 (list-ref p1 i)) (f2 (list-ref p2 i)))
    (printf "COMP ~s C ~s ~s : D ~s ~s~n" i f1 score1 f2 score2)
    (cond ((> f1 f2)
           (set! score1 (add1 score1)))
          ((< f1 f2)
           (set! score2 (add1 score2)))
          (else
           (set! score1 (add1 score1))
           (set! score2 (add1 score2))))))

score1
score2