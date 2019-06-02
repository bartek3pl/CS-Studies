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

;;Zad.3
;prawidłowa implementacja map z typami

(: map (All (a b) (-> (-> a b) (Listof a) (Listof b))))
(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (map f (cdr xs)))))

;prawidłowa implementacja map z kontraktem

;(define/contract (map f xs)
;  (parametric->/c [a b] (-> (-> a b) (listof a) (listof b)))
;  (if (null? xs)
;      null
;      (cons (f (car xs)) (map f (cdr xs)))))

;błędna implementacja map z kontraktem, przechodzi
;ale przez oryginalną wersję zostaje odrzucona

;(define/contract (map-bad f xs)
;  (parametric->/c [a] (-> (-> a a) (listof a) (listof a)))
;  (if (null? xs)
;      null
;      (cons (car xs) (map-bad f (cdr xs)))))

;błędna implementacja map z typem, przechodzi
;ale przez oryginalną wersję zostaje odrzucona [dla procedury (lambda (x) x)]

;(: map-bad (All (a) (-> (-> a a) (Listof a) (Listof a))))
;(define (map-bad f xs)
;  (cons (f (car xs)) (list (car xs))))

;Zmieniona wersja kontraktu nie ogranicza sposobu użytkowania procedury,
;natomiast zmieniona wersja typu ogranicza, bo nie działa dla niektórych procedur
;np. positive? (potrzebuje (-> a a), a dostaje (-> Real Boolean))

;;Zad.5
(struct const ([val : Number]) #:transparent)
(struct (a) op ([symb : Symbol] [l : a] [r : a]) #:transparent)
(struct (a) let-expr ([x : Symbol] [e1 : a] [e2 : a]) #:transparent)
(struct variable ([x : Symbol]) #:transparent)

(define-type (Expr a) (U const
                         (op (Expr a))
                         (let-expr (Expr a))
                         variable))

(: subst (All (a) (-> Symbol Number (Expr a) (Expr a))))
(define (subst x v e)
  (match e
    [(op s l r)   (op s (subst x v l)
                        (subst x v r))]
    [(const n)    (const n)]
    [(variable y) (if (eq? x y)
                      (const v)
                      (variable y))]
    [(let-expr y e1 e2)
     (if (eq? x y)
         (let-expr y
                   (subst x v e1)
                   e2)
         (let-expr y
                   (subst x v e1)
                   (subst x v e2)))]))

(: eval (All (a) (-> (Expr a) Number)))
(define (eval e)
  (match e
    [(const n)    n]
    [(op '+ l r)  (+ (eval l) (eval r))]
    [(op '* l r)  (* (eval l) (eval r))]
    [(let-expr x e1 e2)
     (eval (subst x (eval e1) e2))]
    [(variable n) (error n "cannot reference an identifier before its definition ;)")]))

;(eval (let-expr 'x (const 2) (op '+ (variable 'x) (const 1))))







