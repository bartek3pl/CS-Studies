#lang racket

;;Zad.1
(define (average z y)
  (/ (+ z y) 2))

(define (square x)
  (* x x))

(define (dist x y)
  (abs (- x y)))

(define (close-enough? x y)
  (< (dist x y) 0.00001))

(define sqrt/c
  (->i ([l positive?])
       [result (l) (lambda (x) (close-enough? l (* x x)))]))

(define/contract (my-sqrt x)
  sqrt/c
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? g)
    (< (dist x (square g))
       0.0001))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))  
  (iter 1.0))

;;Zad.2