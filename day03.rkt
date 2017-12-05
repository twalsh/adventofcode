#lang racket

(define test-addresses '(1 12 23 1024))

(define (move dir ptr addr stride len x y)
  (if (= ptr addr)
      (+ (abs x) (abs y))
      (let-values ([(next-dir next-stride next-x next-y)
                    (match dir
                      [ 'left  (values 'down stride (sub1 x) y)]
                      [ 'right (values 'up stride (add1 x) y) ]
                      [ 'up    (values 'left (add1 stride) x (add1 y)) ]
                      [ 'down  (values 'right (add1 stride) x (sub1 y)) ]
                      )])
        (if (= len stride)
            (move next-dir ptr addr next-stride 0 x y)
            (move dir (add1 ptr) addr stride (add1 len) next-x next-y)))))

(define (walk addr)
  (let ((ptr 1) (x 0) (y 0))
    (if (= ptr addr)
        (cons x y)
        (move 'right ptr addr 1 0 x y))))

(for ((addr test-addresses))
  (printf "~a ~a~n" addr (walk addr)))

(walk 361527)