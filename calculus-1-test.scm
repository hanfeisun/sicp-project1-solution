#lang racket/base

(require rackunit
         "calculus-1.scm")

(define (pi-residual x)
  (abs (- 3.1415626535 x)))
(define (near-pi? x) 
     (<  (pi-residual x) 0.01))


(check-equal? (bitfunc 1) -12)
(check-equal? (bitfunc 2) 6)
(check-equal? (bitfunc-rect 2 3) 6)
(check-equal? (bitfunc-rect 2 4) 12)
(check-equal? (bitfunc-rect 2 4.5) 15.0)

(check-pred near-pi? (approx-pi 1000))
(check-pred near-pi? (better-pi 1000))
(check-true (< (pi-residual (better-pi 1000)) (pi-residual (approx-pi 1000))))

(check-equal? (derivative 'x 'x) 1)
(check-equal? (derivative '(+ 12 x) 'x) '(+ 0 1))
(check-equal? (derivative '(+ y x) 'x) '(+ 0 1))
(check-equal? (derivative '(+ x x) 'x) '(+ 1 1))
(check-equal? (derivative '(* 1 x) 'x) '(+ (* 1 1) (* 0 x)))
(check-equal? (derivative '(+ 1 (* 2 x)) 'x) '(+ 0 (+ (* 2 1) (* 0 x))))
(check-equal? (derivative '(* x 4 3) 'x) '(+ (* x (+ (* 4 0) (* 0 3))) (* 1 (* 4 3))))

(check-equal? (derivative '(* x (+ 4 3) x (+ 1 5)) 'x) 
              '(+ (* x (+ (* (+ 4 3) (+ (* x (+ 0 0)) (* 1 (+ 1 5))))
                          (* (+ 0 0) (* x (+ 1 5)))))
                  (* 1 (* (+ 4 3) (* x (+ 1 5))))))


