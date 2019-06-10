#lang racket
;Bartłomiej Hildebrandt
;IIUwr 2018/2019

(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-right f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-right f)))))

;;Zad.1
(define (neg f)
  (list 'neg f))

(define (neg-subf f)
  (cadr f))

(define (conj l r)
  (list 'conj l r))

(define (conj-left f)
  (cadr f))

(define (conj-right f)
  (caddr f))

(define (disj l r)
  (list 'disj l r))

(define (disj-left f)
  (cadr f))

(define (disj-right f)
  (caddr f))

;;Zad.2
(define f (conj (neg (conj 'a 'd)) (disj 'b 'd)))

(define (free-vars f)
  (cond
    [(var? f) (list f)]
    [(neg? f) (free-vars (neg-subf f))]
    [(conj? f) (remove-duplicates (append (free-vars (conj-left f))
                                          (free-vars (conj-right f))))]
    [(disj? f) (remove-duplicates (append (free-vars (disj-left f))
                                          (free-vars (disj-right f))))]))

(define (free-vars-iter f)
  (define (iter f xs)
    (cond
      [(var? f) (remove-duplicates (append xs (list f)))]
      [(neg? f) (iter (neg-subf f) xs)]
      [(conj? f) (iter (conj-right f)
                       (iter (conj-left f) xs))]
      [(disj? f) (iter (disj-right f)
                       (iter (disj-left f) xs))]))
  (iter f null))

;(free-vars f)
;(free-vars-iter f)
;(free-vars-iter (conj (disj (neg 'a) (neg 'c)) (neg 'b)))
;(free-vars-iter (neg (disj (neg 'a) (neg 'c))))

;;Zad.3
(define f2 (neg (conj (neg 'a) 'b)))
(define vxs '((a #f) (b #t)))
(define xs1 '(a b c))

(define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))

;(gen-vals xs1)

(define (get-val x val)
  (if (null? x)
      null
      (second (car (filter (lambda (i) (eq? (car i) x)) val))))) 

;(get-val 'a '((a #f) (b #t)))
;(get-val 'b '((a #f) (b #t)))

(define (eval-formula f vs)
  (cond
    [(var? f) (get-val f vs)]
    [(neg? f) (not (eval-formula (neg-subf f) vs))]
    [(conj? f) (and (eval-formula (conj-left f) vs)
                    (eval-formula (conj-right f) vs))]
    [(disj? f) (or (eval-formula (disj-left f) vs)
                   (eval-formula (disj-right f) vs))]))
 

;(eval-formula f2 vxs)

(define (falsifiable-eval? t)
  (define (helper t vals)
    (if (eval-formula t (car vals))
        (helper t (cdr vals))
        (car vals)))
  (helper t (gen-vals (free-vars t))))

;(falsifiable-eval? (disj 'a (disj 'b 'c)))

;;Zad.4
(define (literal? f)
  (or (var? f)
      (and (neg? f)
           (var? (neg-subf f)))))

;(literal? (neg 'a))

(define (nnf? f)
  (cond
    [(literal? f) #t]
    [(neg? f) #f]
    [(conj? f) (and (nnf? (conj-left f))
                    (nnf? (conj-right f)))]
    [(disj? f) (and (nnf? (disj-left f))
                    (nnf? (disj-right f)))]))

;(nnf? (neg (conj (neg 'a) (neg 'b))))
;(nnf? (conj (neg 'a) (neg 'b)))

;;Zad.5
(define (neg-to-nnf f)
  (cond
    [(var? f) (neg f)]
    [(neg? f) (neg-subf f)]
    [(conj? f) (disj (neg-to-nnf (conj-left f))
                     (neg-to-nnf (conj-right f)))]
    [(disj? f) (conj (neg-to-nnf (disj-left f))
                     (neg-to-nnf (disj-right f)))]))

(define (convert-to-nnf f)
  (cond
    [(var? f) f]
    [(neg? f) (neg-to-nnf (neg-subf f))]
    [(conj? f) (conj (convert-to-nnf (conj-left f))
                     (convert-to-nnf (conj-right f)))]
    [(disj? f) (disj (convert-to-nnf (disj-left f))
                     (convert-to-nnf (disj-right f)))]))
    
;(convert-to-nnf (neg (conj (neg (conj 'a 'd)) (disj 'b 'd))))

;;Zad.6
(define (cnf? f)
  (cond
    [(literal? f) #t]
    [(neg? f) #f]
    [(conj? f) #t]
    [(disj? f) (and (cnf? (disj-left f))
                    (cnf? (disj-right f))
                    (not (conj? (disj-left f)))
                    (not (conj? (disj-right f))))]
    ))

(define (cnf l r)
  (list 'cnf l r))

(define (cnf-left f)
  (cadr f))

(define (cnf-right f)
  (caddr f))

(define (convert-to-cnf f)
  (cond
    [(var? f) f]
    [(neg? f) (convert-to-cnf (neg-to-nnf (neg-subf f)))]
    [(conj? f)
     (cond
       [(cnf? f) f]
       [else (conj (convert-to-cnf (conj-left f))
                   (convert-to-cnf (conj-right f)))]
       )]
    [(disj? f)
     (cond
       [(cnf? f) f]
       [(literal? (disj-left f)) (conj (convert-to-cnf (disj (disj-left f)
                                                             (conj-left (convert-to-cnf (disj-right f)))))
                                       (convert-to-cnf (disj (disj-left f)
                                                             (conj-right (convert-to-cnf (disj-right f))))))]
       [(literal? (disj-right f)) (conj (convert-to-cnf (disj (conj-left (convert-to-cnf (disj-left f)))
                                                              (disj-right f)))
                                        (convert-to-cnf (disj (conj-right (convert-to-cnf (disj-left f)))
                                                              (disj-right f))))]
       [else (conj (convert-to-cnf (disj (conj (disj-left (convert-to-cnf (disj-left f)))
                                               (disj-right (convert-to-cnf (disj-left f))))
                                         (disj-left (convert-to-cnf (disj-right f)))))
                                   (disj-right (convert-to-cnf (disj-right f))))])] 
    ))
      