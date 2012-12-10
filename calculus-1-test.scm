#lang racket/base

(require rackunit
         "calculus-1.scm")

(check-equal? (bitfunc 1) -12)
(check-equal? (bitfunc 2) 6)


(check-equal? (bitfunc-rect 2 3) 6)
(check-equal? (bitfunc-rect 2 4) 12)
(check-equal? (bitfunc-rect 2 4.5) 15.0)

(define (near-pi? x) 
  (let ([residual (abs (- 3.14 x))])
     (<  residual 0.01)))

(check-pred near-pi? (approx-pi 1000))
(check-pred near-pi? (better-pi 1000))


