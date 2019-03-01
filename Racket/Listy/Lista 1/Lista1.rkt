#lang racket
;;Bartłomiej Hildebrandt IIUwr 2018/2019

;; Zad.2
(define prefix
  (/ (+ 5 4 (- 2 (- 3 (+ 6 0.8))))
     (* 3 (- 6 2) (- 2 7))))

;; Zad.4
(define (square a)
  (* a a))

(define (squaresSum a b c)
  (cond 
    [(>= a b)
     (+ (square a) (if (> b c) (square b) (square c)))]
    [(>= b a)
     (+ (square b) (if (> a c) (square a) (square c)))]))

;; Zad.5
; Jeśli b>0 to zwraca +, wpp - i wykonuje operacje na a i b

;; Zad.6
; gdyby and był proc. wbud. wystąpiłby błąd 1/0
(and #f (/ 1 0))
; gdyby or był proc. wbud. wystąpiłby błąd 1/0
(or #t (/ 1 0))

;; Zad.7
;A
(define (p) (p))
;B
(define (test x y)
  (if (= x 0)
      0
      y))

;W A dla stosowanej błąd, dla normalnej nieskończone wywołania

;W B dla (test (* 2 5) (- 3 1)
;normalna
;1. (test (* 2 5) (- 3 1))
;2. (if (= 0 (* 2 5)) (* 2 5) (- 3 1)))
;3. (if (= 0 10) (* 2 5) (- 3 1)))
;4. (* 2 5)
;5. 10

;stosowana
;1. (test (* 2 5) (- 3 1))
;2. (if (= 0 (* 2 5)) (* 2 5) (- 3 1)))
;3. (if (= 0 10) 10 2))
;4. 10

;; Zad.8
(define (my-expt a b)
  (define (helper res b a)
    (if (<= b 0)
        res
        (helper (* res a) (- b 1) a)))
  (helper 1 b a))

(define (power-close-to b n)
  (define (helper b n e)
    (if (> (my-expt b e) n)
        e
        (helper b n (+ e 1))))
  (helper b n 1))









         