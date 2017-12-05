#lang racket

(define test-addresses '(1 12 23 1024))

(define (right ptr addr stride len x y)
 ; (printf "R ~a ~a | ~a ~a | ~a ~a ~n" addr ptr x y stride len)
  (if (= ptr addr)
      (cons x y)
      (if (= len stride)
          (up ptr addr stride 0 x y)
          (right (add1 ptr) addr stride (add1 len) (add1 x) y))))

(define (left ptr addr stride len x y)
;  (printf "L ~a ~a ~a ~a ~a ~n" ptr x y stride len)
  (if (= ptr addr)
      (cons x y)
      (if (= len stride)
          (down ptr addr stride 0 x y)
          (left (add1 ptr) addr stride (add1 len) (sub1 x) y))))

(define (down ptr addr stride len x y)
;  (printf "D ~a ~a ~a ~a ~a ~n" ptr x y stride len)
  (if (= ptr addr)
      (cons x y)
      (if (= len stride)
          (right ptr addr (add1 stride) 0 x y)
          (down (add1 ptr) addr stride (add1 len) x (sub1 y)))))

(define (up ptr addr stride len x y)
 ; (printf "U ~a ~a | ~a ~a | ~a ~a ~n" addr ptr x y stride len)
  (if (= ptr addr)
      (cons x y)
      (if (= len stride) ; end of y-line - turn left 
          (left ptr addr (add1 stride) 0 x y)
          ; else keep going up
          (up (add1 ptr) addr stride (add1 len) x (add1 y)))))

(define (walk addr)
  (define coord
  (let ((ptr 1) (x 0) (y 0))
    (if (= ptr addr)
        (cons x y)
        (right ptr addr 1 0 x y))))
  (+ (abs (car coord)) (abs (cdr coord))))

(for ((addr test-addresses))
  (printf "~a ~a~n" addr (walk addr)))

(walk 361527)

  