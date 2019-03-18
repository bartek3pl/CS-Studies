#lang racket
;Bartłomiej Hildebrandt
;IIUwr 2018/2019

(define (square x)
  (* x x))

;;Różne procedury na listach

(define (list? xs)
  (or (null? xs)
      (and (cons? xs)
           (list? (cdr xs)))))

;(list? '(1 2 3))
;(list? (lambda(x) x))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

;(append '(1 2 3) '(4 5 6))

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

;(length '(2 3 5))

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

;(map (lambda(x) (* x x x)) '(1 2 3 4))

(define (filter p xs)
  (if (null? xs)
      null
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
          (filter p (cdr xs)))))

;(filter (lambda(x) (even? x)) '(1 2 3 4 5 6 7 8))

;; Zad.1
(define (vect? v)
  (and (cons? v)
       (= (length v) 2)
       (cons? (car v))
       (cons? (cdr v))))

;(vect? '((1 2) (6 8)))

(define (make-vect v1 v2)
  (cons v1 (list v2)))

;(make-vect '(1 2) '(6 8))

(define (vect-begin v)
  (car v))

(define (vect-end v)
  (cadr v))

(define (point? x)
  (and (pair? x)
       (= (length x) 2)))

;(point? '(a (b c)))

(define (make-point x y)
  (cons x (list y)))

(define (point-x p)
  (car p))

(define (point-y p)
  (cadr p))

;(point-y (vect-begin (make-vect '(1 4) '(5 6))))
;(point-y (vect-end (make-vect '(1 4) '(5 6))))

(define (vect-length v)
  (sqrt (+ (square (- (point-x (vect-begin v)) (point-y (vect-begin v))))
           (square (- (point-x (vect-end v)) (point-y (vect-end v)))))))

;(vect-length (make-vect '(1 4) '(5 6)))

(define (vect-scale v k)
  (make-vect (vect-begin v)
             (make-point (* k (point-x (vect-end v)))
                         (* k (point-y (vect-end v))))))

;(vect-scale (make-vect '(1 4) '(5 6)) 3)

(define (vect-translate v p)
  (make-vect p (make-point
                (+ (point-x p) (- (point-x (vect-end v)) (point-x (vect-begin v))))
                (+ (point-y p) (- (point-y (vect-end v)) (point-y (vect-begin v)))))))

;(vect-translate (make-vect (make-point 1 0) (make-point 3 4)) (make-point 3 4))

;; Zad.2
(define (make-vect-deg p deg len)
  (list p deg len))

;(make-vect-deg (make-point 1 2) 30 5)

(define (vect-deg? v)
  (and (list? v)
       (cons? (car v))
       (= (length v) 3)
       (and (< (caddr v) (* 2 3.14))
            (> (caddr v) 0))))

;(vect-deg? (make-vect-deg (make-point 1 2) 30 5))

(define (vect-deg-point v)
  (car v))

(define (vect-deg-begin v)
  (caar v))

(define (vect-deg-end v)
  (second (car v)))

(define (vect-deg-deg v)
  (cadr v))

(define (vect-deg-len v)
  (caddr v))

;(vect-deg-begin (make-vect-deg (make-point 1 2) 30 5))

(define (vect-deg-length v)
  (vect-deg-len v))

(define (vect-deg-scale v k)
  (make-vect-deg (vect-deg-point v) (vect-deg-deg v) (* (vect-deg-len v) k)))

;(vect-deg-scale (make-vect-deg (make-point 1 2) 30 5) 2)

;; Zad.3
(define (reverse-rec xs)
  (if (null? xs)
      null
      (append (reverse-rec (cdr xs)) (list (car xs)))))

;Złożoność czasowa: O(n)
;Złożoność pamięciowa: O(n)

(define (reverse-iter xs)
  (define (helper xs acc)
    (if (null? xs)
        acc
        (helper (cdr xs) (cons (car xs) acc))))
  (helper xs null))

;Złożoność czasowa: O(n)
;Złożoność pamięciowa: O(1)

;(reverse-rec '(1 2 3 4 5))
;(reverse-iter '(1 2 3 4 5))

;; Zad.4
(define (insert xs n)
  (if (null? xs)
      (list n)
      (if (< n (car xs))
          (cons n xs)
          (cons (car xs) (insert (cdr xs) n)))))

;(insert '(1 2 3 4 6 7 8) 5)

(define (insert-sort xs)
  (if (null? xs)
      '()
      (insert (insert-sort (cdr xs)) (car xs))))

;(insert-sort '(5 3 2 4 6 1))

;; Zad.7
(define (concat xs)
  (if (null? xs)
      '()
      (append (car xs) (concat (cdr xs)))))

;(concat '((1 2 3) (4 5 6) (7 8 9) (a b c)))

;; Zad.6
(define (insert-perm xs n)
  (if (null? xs)
      (list (list n))
      (cons (cons n xs)
            (map (lambda(ys) (cons (car xs) ys))
                 (insert-perm (cdr xs) n)))))

(define (perm xs)
  (if (null? xs)
      '(())
      (concat (map (lambda(ys) (insert-perm ys (car xs)))
                   (perm (cdr xs))))))

;(perm '(1 2 3 4 5 6))








