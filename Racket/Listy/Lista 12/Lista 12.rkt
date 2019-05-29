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
;kontrakt parametryczny
(define/contract (filter1 f xs)
  (parametric->/c [a]
                  (-> (-> a boolean?) (listof a) (listof a)))
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

;kontrakt zależny
(define/contract (contains l1 l2)
  (-> list? list? boolean?)
  (if (andmap (lambda (x) (member x l1)) l2)
      #t
      #f))

(define/contract (filter2 f xs)
  (->i ([p (-> any/c boolean?)]
        [l list?])
       [result list?]
       #:post (l result)
       (contains l result))
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

;;Zad.3
;sygnatura monoidów z prostymi kontraktami
(define-signature monoid^
  ((contracted
    [elem? (-> any/c boolean?)]
    [neutral elem?]
    [oper (-> elem? elem? elem?)])))

;implementacja komponentu - liczby całkowite z zerem i dodawaniem
(define-unit monoid1@
  (import)
  (export monoid^)

  (define (elem? e)
    (integer? e))

  (define neutral 0)

  (define (oper a b)
    (+ a b))
  )

;(define-values/invoke-unit/infer monoid1@)

;(elem? 5)
;neutral
;(oper 7 9)

;implementacja komponentu - listy z listą pustą i scalaniem list
(define-unit monoid2@
  (import)
  (export monoid^)

  (define (elem? e)
    (or (null? e)
        (list? e)))
             
  (define neutral '())

  (define (oper xs ys)
    (append xs ys))
  )

;(define-values/invoke-unit/infer monoid2@)

;(elem? '(1 2 3))
;neutral
;(oper '(1 2) '(3 4))

;;Zad.4
(require quickcheck)

;Dla liczb
;(define-values/invoke-unit/infer monoid1@)
;Dla list
;(define-values/invoke-unit/infer monoid2@)

;Dla liczb
;(quickcheck
; (property ([x arbitrary-integer]
;            [y arbitrary-integer])
;           (eq? x (oper x neutral))
;           (eq? x (oper neutral x))
;           (eq? (oper (oper x y) y) (oper x (oper y y)))))
;
;Dla list
;(quickcheck
; (property ([xs (arbitrary-list arbitrary-symbol)]
;            [ys (arbitrary-list arbitrary-symbol)])
;           (eq? xs (oper xs neutral))
;           (eq? xs (oper neutral xs))
;           (equal? (oper (oper xs ys) ys) (oper xs (oper ys ys)))))

;;Zad.5
(define-signature set^
  ((contracted
    [empty-set null?]
    [set? (-> any/c boolean?)]
    [empty-set? (-> set? boolean?)]
    [member? (-> integer? set? boolean?)]
    [set-sing (-> integer? set?)]
    [set-union (-> set? set? set?)]
    [set-inter (-> set? set? set?)]
    )))

(define-unit set@
  (import)
  (export set^)

  (define empty-set null)
  
  (define (set? s)
    (or (empty-set? s)
        (and (list? s)
             (andmap integer? s))))

  (define (empty-set? s)
    (eq? s empty-set))

  (define (member? int s)
    (if (member int s) #t #f))

  (define (set-sing int)
    (list int))

  (define (set-union s1 s2)
    (remove-duplicates (append s1 s2)))

  (define (set-inter s1 s2)
    (if (empty-set? s1)
        empty-set
        (if (member (car s1) s2)
            (cons (car s1) (set-inter (cdr s1) s2))
            (set-inter (cdr s1) s2))))
  )
            
(define-values/invoke-unit/infer set@)

;empty-set
;(empty-set? null)
;(set? '(1 2 3))
;(member? 2 '(1 2 3))
;(set-sing 42)
;(set-union '(1 2 3) '(3 4 5))
;(set-inter '(1 2 3) '(3 4 5))


