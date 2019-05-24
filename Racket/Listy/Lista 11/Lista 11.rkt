#lang racket

(require racket/contract/parametric)

;;Zad.1
(define/contract (suffixes xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (define (helper xs acc)
    (if (null? xs)
        (cons acc xs)
        (cons acc
              (helper (cdr xs) (append acc (list (car xs)))))))  
  (helper xs null))

;(suffixes '(1 2 3 4))

;;Zad.2
(define (sublists xs)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? xs)
      (list null)
       (append-map
        (lambda (ys) (cons (cons (car xs) ys) (list ys)))
        (sublists (cdr xs)))))

;(sublists '(1 2 3))

;;Zad.3
(define/contract (test1 x y)
  (parametric->/c [a b] (-> a b a))
  (if (not (eq? x y))
      x
      0))

;;Zad.5
(define/contract (foldl-map f a xs)
  (parametric->/c [a b]
                  (-> (-> a b (cons/c a b))
                      b (listof a) (cons/c (listof a) b)))
  (define (it a xs ys)
    (if (null? xs)
        (cons (reverse ys) a)
        (let [(p (f (car xs) a))]
          (it (cdr p)
              (cdr xs)
              (cons (car p) ys)))))
  (it a xs null))

;(foldl-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3 4))
 
