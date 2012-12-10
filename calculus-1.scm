#lang racket

"Part 1: Numerical integration"


"Problem 1: Bitdiddle's function"

(define (bitfunc x)
  (- (+ (expt x 4)  
        (expt x 2)) 
     14))

"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2)
  (* (bitfunc x1)
     (- x2 x1)))

"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-recur num-steps x1 x2)
  (let ([step-width (/ (- x2 x1) num-steps)])    
    (define (recr current-x)
      (let ([next-x (+ current-x step-width)])
        (if (>= current-x x2) 
            0
            (+ (bitfunc-rect current-x next-x)
               (recr next-x)))))
    (recr x1)))


(define (bitfunc-integral-iter num-steps x1 x2) 
  (let ([step-width (/ (- x2 x1) num-steps)])      
    (define (iter current-integral current-x)
      (let ([next-x (+ current-x step-width)])
        (if (>= current-x x2) 
            current-integral 
            (iter (+ current-integral 
                     (bitfunc-rect current-x next-x)) 
                  next-x))))
    (iter 0 x1)))


"Problem 4: Integrating any function"

(define (func-rect func x1 x2)
  (* (func x1)
     (- x2 x1)))

(define (integral func num-steps x1 x2)
  (let ([step-width (/ (- x2 x1) num-steps)])     
    (define (iter current-integral current-x)
      (let ([next-x (+ current-x step-width)])
        (if (>= current-x x2) 
            current-integral
            (iter (+ current-integral 
                     (func-rect func current-x next-x))
                  next-x))))
    (iter 0 x1)))


"Problem 5: Area of a unit circle"


(define (approx-pi num-steps)
  (* (integral (lambda (x) (sqrt (- 1 (* x x)))) num-steps 0 1) 
     4))


"Problem 6: Integrating with pieces of any shape"

(define rectangle func-rect) 

(define (trapezoid func x1 x2)
  (/ (* (+ (func x1) 
           (func x2)) 
        (- x2 x1)) 
     2))

(define (integral-with piece func num-steps x1 x2)
  (let ([step-width (/ (- x2 x1) num-steps)])     
    (define (iter current-integral current-x)
      (let ([next-x (+ current-x step-width)])
        (if (>= current-x x2) 
            current-integral
            (iter (+ current-integral 
                     (piece func current-x next-x))
                  next-x))))
    (iter 0 x1)))


"Problem 7: Better approximation of pi"
(define (better-pi num-steps)
  (* (integral-with trapezoid (lambda (x) (sqrt (- 1 (* x x)))) num-steps 0 1)
     4))


"Part 2: Symbolic differentiation"


; Basic Abstraction
(define (variable? x) (symbol? x))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2) (list '+ a1 a2))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product a1 a2) (list '* a1 a2))

(define (deriv-constant constant wrt)
  0)

"Problem 8: Derivative of a variable"

(define (deriv-variable var wrt)
  (if (eq? var wrt)
      1
      0))

"Problem 9: Calling the right function"


(define (derivative expr wrt)
  (let ([e (decompose expr)])
        (cond [(number? e) (deriv-constant e wrt)]
              [(variable? e) (deriv-variable e wrt)]
              [(sum? e) (deriv-sum e wrt)]
              [(product? e) (deriv-product e wrt)]
              [else (error "Don't know how to differentiate" expr)])))


"Problem 10: Derivative of a sum"

(define (deriv-sum expr wrt)
  (make-sum (derivative (addend expr) wrt)
            (derivative (augend expr) wrt)))

"Problem 11: Derivative of a product"

(define (deriv-product expr wrt)
  (make-sum
   (make-product (multiplier expr)
                 (derivative (multiplicand expr) wrt))
   (make-product (derivative (multiplier expr) wrt)
                 (multiplicand expr))))

"Problem 12: Additional testing"

; Additional test cases for 'derivative' go here.

(define (all-but-last l) (reverse (cdr (reverse l))))

(define (decompose x)
  (cond [(number? x) x]
        [(symbol? x) x]
        [(pair? x)
         (let ([operator (car x)]
               [operands (cdr x)])
           (if (<= (length operands) 2) x
               (foldr (lambda (v l) 
                        (list operator (decompose v) (decompose l)))
                      (last operands) 
                      (all-but-last operands))))]))



(provide bitfunc bitfunc-rect approx-pi better-pi derivative)