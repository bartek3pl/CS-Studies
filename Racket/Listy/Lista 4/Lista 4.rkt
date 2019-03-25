#lang racket
;Bartłomiej Hildebrandt
;IIUwr 2018/2019

;;Przydatne procedury
(define (tagged-list? len sym xs)
  (and (list? xs)
       (eq? (car xs) sym)
       (= (length xs) len)))

;Drzewa binarne etykietowane w wierzchołkach wewnętrznych

(define leaf 'leaf)

(define (leaf? x)
  (eq? x 'leaf))

(define (make-node e l r)
  (list 'node e l r))

(define (node? x)
  (tagged-list? 4 'node x))

(define (node-elem x)
  (second x))
  
(define (node-left x)
  (third x))

(define (node-right x)
  (fourth x))

(define (tree? x)
  (or (leaf? x)
      (and (node? x)
           (tree? (node-left x))
           (tree? (node-right x)))))

;Operacje na drzewach BST
(define tree
  (list 'node 7 (list 'node 3 (list 'node 1 leaf leaf) (list 'node 5 leaf leaf))
                (list 'node 9 (list 'node 8 leaf leaf) (list 'node 11 leaf leaf))))

(define (find x t)
  (cond
    [(leaf? t) #f]
    [(= (node-elem t) x) #t]
    [(> (node-elem t) x) (find x (node-left t))]
    [(< (node-elem t) x) (find x (node-right t))]))

;(find 11 tree)

(define (insert x t)
  (cond
    [(leaf? t) (make-node x leaf leaf)]
    [(= (node-elem t) x) t]
    [(> (node-elem t) x) (make-node (node-elem t)
                                    (insert x (node-left t))
                                    (node-right t))]
    [(< (node-elem t) x) (make-node (node-elem t)
                                    (node-left t)
                                    (insert x (node-right t)))]))

;(insert 2 tree)

;;Zad.1
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (concat xs)
  (if (null? xs)
      null
      (append (car xs) (concat (cdr xs)))))

;(concat '( (1 2 3) (4 5 6) (7 8 9) ))

;;Zad.2
(define (mirror t)
  (if (leaf? t)
      'leaf
      (make-node (node-elem t) (node-right t) (node-left t))))

;(mirror (mirror tree))

;;Zad.3
(define (flatten t)
  (define (helper t acc)
    (if (leaf? t)
        acc
        (helper (node-left t) (cons (node-elem t)
                                    (helper (node-right t) acc)))))
  (helper t null))

;(flatten tree)

;;Zad.4
(define (treesort-helper xs)
  (if (null? xs)
      leaf
      (insert (car xs)
              (treesort-helper (cdr xs)))))

(define (treesort xs)
  (flatten (treesort-helper xs)))
                   
;(treesort-helper '(7 3 1 5 9 11 8))
;(treesort '(7 3 1 5 9 11 8))







