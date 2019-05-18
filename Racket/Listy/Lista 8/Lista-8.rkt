#lang racket

(struct variable (x) #:transparent)
(struct const (n) #:transparent)
(struct op (symb a b) #:transparent)
(struct let-expr (x e1 e2) #:transparent)
(struct if-expr (cond e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(variable x) (symbol? x)]
    [(const n) (number? n)]
    [(op symb a b)
     (and (member symb '(+ - * /))
          (number? a)
          (number? b))]
    [(let-expr x e1 e2)
     (and (symbol? x)
          (expr? e1)
          (expr? e2))]
    [(if-expr cond e1 e2)
     (andmap (expr? (list cond e1 e2)))]
    [_ #f]
    ))

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier")]
    [(eq? (caar xs) x)
     (cadar xs)]
    [else
     (lookup x (cdr xs))]
    ))

(define (op-to-proc x)
  (lookup x `(
              (+,+)
              (-,-)
              (*,*)
              (/,/)
              (>,>)
              (>=,>=)
              (<,<)
              (<=,<=)
              (%, modulo)
              (==, (lambda (x y) (= x y)))
              (!=, (lambda (x y) (not (= x y))))
              (&&, (lambda (x y) (and x y)))
              (||, (lambda (x y) (or x y)))
              )))

(define (env-empty) null)
(define env-lookup lookup)

(define (env-add x v env)
  (cons (list x v) env))

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r) ((op-to-proc s) (eval l env)
                                (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]))

(define (run e)
  (eval e (env-empty)))

(define test
  (let-expr 'x (op '* (const 4) (const 3))
            (op '- (variable 'x) (const 2))))

;;WHILE

(struct skip () #:transparent) ; skip
(struct comp (s1 s2) #:transparent) ; s1; s2
(struct assign (x e) #:transparent) ; x := e
(struct while (b s) #:transparent) ; while (b) s
(struct if-stm (b e1 e2) #:transparent) ; if (b) e1 else e2
(struct var-block (x e s) #:transparent) ; var x := e in s

(define (stm? e)
  (match e
    [(skip) #t]
    [(comp s1 s2)
     (and (stm? s1)
          (stm? s2))]
    [(assign x e)
     (and (symbol? x)
          (expr? e))]
    [(while b s)
     (and (expr? b)
          (stm? s))]
    [(if-stm b e1 e2)
     (and (expr? b)
          (stm? e1)
          (stm? e2))]
    [_ #f]
    ))

(define (update x v xs)
  (cond
    [(null? xs)
     (error x "unknown identifier")]
    [(eq? (caar xs) x)
     (cons (list x v) (cdr xs))]
    [else
     (cons (car xs) (update x v (cdr xs)))]
    ))

(define env-update update)
(define env-discard cdr)
(define (env-from-assoc-list xs) xs)

(define (interp p m) ; p-procedura, m-stan pamięci
  (match p
    [(skip) m]
    [(comp s1 s2)
     (interp s2 (interp s1 m))]
    [(assign x e)
     (env-update x (eval e m) m)]
    [(while b s)
     (if (eval b m)
         (interp p (interp s m))
         m)]
    [(var-block x e s)
     (env-discard
      (interp s (env-add x (eval e m) m)))]
    [(if-stm b e1 e2)
     (if (eval b m)
         (interp e1 m)
         (interp e2 m))]
    ))


;;Zad.1
(define (list-to-cycle xs)
  (define (helper xs fst)
    (if (null? xs)
        fst
        (mcons (car xs) (helper (cdr xs) fst))))
  (let ((start (mcons (car xs) null))) 
        (begin
          (set-mcdr! start (helper (cdr xs) start))
          start)))

(define testlist '(1 2 3 4))

;;Zad.2
(define (set-nth! n xs v)
  (if (null? xs)
      '()
      (if (> n 0)
          (mcons (mcar xs)
                 (set-nth! (- n 1) (mcdr xs) v))
          (begin (set-mcar! xs v)
                 xs))))
      
(define testmlist (mcons 1 (mcons 2 (mcons 3 (mcons 4 '())))))

;;Zad.3
;factorial
(define fact-in-WHILE
  (var-block 'x (const 0)
             (comp (assign 'x (const 1))
                   (comp (while (op '> (variable 'i) (const 0))
                                (comp (assign 'x (op '* (variable 'x) (variable 'i)))
                                      (assign 'i (op '- (variable 'i) (const 1)))))
                         (assign 'i (variable 'x))))))

(define (fact n)
  (env-lookup 'i (interp fact-in-WHILE `( (i, n) ))))

;fibonacci
(define fib-in-WHILE
  (var-block 'a (const 0)
             (var-block 'b (const 1)
                        (comp (while (op '> (variable 'n) (const 0))
                                     (comp (assign 'b (op '+ (variable 'b) (variable 'a)))
                                           (comp (assign 'a (op '- (variable 'b) (variable 'a)))
                                                 (assign 'n (op '- (variable 'n) (const 1))))))
                              (assign 'n (variable 'b))))))

(define (fib n)
  (env-lookup 'n (interp fib-in-WHILE `( (n, (- n 1)) ))))

;;Zad.4

   