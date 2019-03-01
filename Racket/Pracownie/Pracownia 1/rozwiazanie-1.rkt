#lang racket
;;Bartłomiej Hildebrandt IIUwr 2018/2019

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (dist x y)
  (abs (- x y)))

(define (count x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cube-root x)
  (define (improve approx)
    (count x approx))
  (define (good-enough? approx)
    (< (dist x (cube approx)) 0.0000000001))
  (define (sqrt3 approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (sqrt3 (improve approx))]))
  (sqrt3 1.0))

;; Tests
(cube-root 2)
(cube-root 3)
(cube-root 64)
(cube-root (+ 4 4))
(cube-root 0.125)
(cube-root 0)
(cube-root -3)

  

