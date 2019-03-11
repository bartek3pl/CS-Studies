#lang racket
;Bartłomiiej Hildebrandt
;IIUwr 2018/2019

;; Zad.1
;x - wolna

;(let ([x 3])
;     (+ x y)) -> x - związany w let, y - wolny

;(let ([x 1]
;     (y (+ x 2)])
;     (* x y))) -> x - związany w let, y - związany

;(let ([x 1])
;  (let ([y (+ x 2)]) -> x - związany w 1 let
;     (* x y))) -> x - związany w 2 let, y - związany w 2 let

;(lambda (x y)
;   (* x y z)) -> x - związany, y - związany, z - wolny

;(let ([x 1])
;  (lambda (y z)
;    (* x y z))) -> x - związany w let, y - związany w lambda, z - związany w lambda

;; Zad.2
(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f x)
    (g (f x))))

;((compose square inc) 5)

;; Zad.3
(define (identity x)
  x)

(define (repeated p n)
  (if (> n 0)
    (compose p (repeated p (- n 1)))
    (identity p)))

;((repeated (lambda(x) (square x)) 2) 2)

;; Zad.4
;procedura z wykładu
(define (sum val next start end)
  (if (> start end)
      0
      (+ (val start)
         (sum val next (next start) end))))

;(sum inc inc 1 5)

;procedura generująca proces rekurencyjny
(define (product-rec val next start end)
  (if (> start end)
      1.0
      (* (/ (next start) val) 
         (if (< (next start) val)
             (product-rec val next (next start) end)
             (product-rec (next val) next start end)))))

;procedura generująca proces iteracyjny
(define (product-iter val next start end)
  (lambda (acum)
    (if (> start end)
        acum
        (if (< (next start) val)
            ((product-iter val next (next start) end) (* acum (/ (next start) val)))
            ((product-iter (next val) next start end) (* acum (/ (next start) val)))))))
                        
(define pi_rec
  (* 4 (product-rec 3 (lambda(x) (+ x 2)) 0 50000)))

(define pi_iter
  (* 4 ((product-iter 3 (lambda(x) (+ x 2)) 0 50000) 1.0)))

pi_rec
pi_iter

;; Zad.6
(define (cont-frac-rec num den k)
  (define (helper i)
    (if (>= i k)
        1.0
        (/ (num i) (+ (den i) (helper (+ i 1))))))
  (helper 1.0))

(define (cont-frac-iter num den k)
  (define (helper i acum)
    (if (= i 0)
        acum
        (helper (- i 1) (/ (num i) (+ (den i) acum)))))
  (helper k 1.0))

;(cont-frac-rec (lambda(i) 1.0) (lambda(i) 1.0) 100)
;(cont-frac-iter (lambda(i) 1.0) (lambda(i) 1.0) 100)

;; Zad.7
(define (pi-frac k)
   (+ 3.0 (cont-frac-rec
           (lambda(i) (square (- (* i 2) 1))) (lambda(i) 6.0) k)))

;(pi-frac 100)

;; Zad.8
(define (atan-cf x k)
  (/ x (+ 1 (cont-frac-rec
             (lambda(i) (square (* i x))) (lambda(i) (+ (* i 2) 1)) k))))

;(atan-cf 1 1000)
;(atan 1) -> procedura wbudowana (dla testu)

;; Zad.9
(define (build n d b)
  (/ n (+ d b)))





















