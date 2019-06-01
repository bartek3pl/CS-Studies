#lang typed/racket

;;Zad.1
(: prefixes (All (a) (-> (Listof a) (Listof (Listof a)))))
(define (prefixes xs)
  (: helper (All (a) (-> (Listof a) (U (Listof a) Null) (Listof (Listof a)))))
  (define (helper xs acc)
    (if (null? xs)
        (cons acc xs)
        (cons acc
              (helper (cdr xs) (append acc (list (car xs)))))))  
  (helper xs null))

;;Zad.2
(struct vector2 ([x : Real] [y : Real]) #:transparent)
(struct vector3 ([x : Real] [y : Real] [z : Real]) #:transparent)

(: square (-> Real Real))
(define (square x)
  (* x x))

(: vector-length-cond (-> (U vector2 vector3) Number))
(define (vector-length-cond v)
  (cond
    [(vector2? v) (sqrt (+ (square (vector2-x v)) (square (vector2-y v))))]
    [(vector3? v) (sqrt (+ (square (vector3-x v)) (square (vector3-y v)) (square (vector3-z v))))]
    [else 0]
    ))

(: vector-length-match (-> (U vector2 vector3) Number))
(define (vector-length-match v)
  (match v
    [(vector2 x y)
     (sqrt (+ (square x) (square y)))]
    [(vector3 x y z)
     (sqrt (+ (square x) (square y) (square z)))]
    ))
