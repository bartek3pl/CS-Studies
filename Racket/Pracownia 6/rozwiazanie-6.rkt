#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))

;; wyrażenie złożone z: dziura | stała | operacja 2-argumentowa | let | zmienna

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t) ;; sprawdza czy w wyrażeniu jest jedna dziura
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  (define (helper e acc)
    (cond
      ((hole? e) acc)
      
      ((and (binop? e)
            (arith/let/hole-expr? (binop-left e)))
           (helper (binop-left  e) acc))
      ((and (binop? e)
            (arith/let/hole-expr? (binop-right e)))
           (helper (binop-right  e) acc))
      
      ((let? e)
       (if (arith/let/hole-expr? (let-def-expr (let-def e)))
           (helper (let-def-expr (let-def e)) acc)
           (helper (let-expr e)
                   (if (member (let-def-var (let-def e)) acc)
                       acc
                       (cons (let-def-var (let-def e)) acc)))))))
            
    (helper e null))

;; TESTY

(define (test)

  (define tests
    (list
     (list '(let (x 3) (let(y 7) (+ x hole))) '(y x))
     (list '(let (x 3) (let(y hole) (+ x 3))) '(x))
     (list '(+ 3 hole) '())
     (list '(let (x hole) (+ (+ x 3) y)) '())
     (list '(+ (let(x 4) 5) hole) '())
     (list '(- (let (y (let (x hole) 1)) 2) 5) '())
     (list '(let (chomik  1)
              (let (kotek  2)
                (let (chomik 3)
                  (let (piesek 4) hole)))) '(piesek kotek chomik))))
  
  (define (hole-context-test t)
    (cond
      ((null? t) #t)
      ((equal? (hole-context (caar t)) (cadr (car t))) 
       (hole-context-test (cdr t)))
      (else #f)))

  (hole-context-test tests))


(test)

