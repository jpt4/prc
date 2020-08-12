#lang racket/base
(require redex/reduction-semantics)
(require redex/pict)

;s1.1

(define-language L
  (e (e e)
     (lambda (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e))
  (t (-> t t) num)
  (x variable-not-otherwise-mentioned))

;e1
;"amb.rkt"> (redex-match L ((lambda (e_arg t_typ) e_body) e_op) (term ((lambda (x num) (+ x 1)) 17)))
#;(list
 (match
     (list
      (bind 'e_arg 'x)
      (bind 'e_body '(+ x 1))
      (bind 'e_op 17)
         (bind 't_typ 'num))))

;e2
;"amb.rkt"> (redex-match L (-> t_d t_r) (term (-> num (-> num num))))
#;(list (match (list (bind 't_d 'num) (bind 't_r '(-> num num)))))

;e3
;"amb.rkt"> (redex-match L (e_0 ... e_1 e_2 e_4 ...) (term (1 2 3 4)))
#;(list
 (match (list (bind 'e_0 '()) (bind 'e_1 1) (bind 'e_2 2) (bind 'e_4 '(3 4))))
 (match (list (bind 'e_0 '(1)) (bind 'e_1 2) (bind 'e_2 3) (bind 'e_4 '(4))))
 (match (list (bind 'e_0 '(1 2)) (bind 'e_1 3) (bind 'e_2 4) (bind 'e_4 '()))))

;e4
;"amb.rkt"> (redex-match L (e_1 ..._1 e_left e_2 ..._2 e_center e_3 ..._2 e_right e_4 ..._1) (term (1 2 3 4 5 6 7)))
#;(list
 (match
     (list
      (bind 'e_1 '())
      (bind 'e_2 '(2 3))
      (bind 'e_3 '(5 6))
      (bind 'e_4 '())
      (bind 'e_center 4)
      (bind 'e_left 1)
      (bind 'e_right 7)))
 (match
     (list
      (bind 'e_1 '(1))
      (bind 'e_2 '(3))
      (bind 'e_3 '(5))
      (bind 'e_4 '(7))
      (bind 'e_center 4)
      (bind 'e_left 2)
      (bind 'e_right 6)))
 (match
     (list
      (bind 'e_1 '(1 2))
      (bind 'e_2 '())
      (bind 'e_3 '())
      (bind 'e_4 '(6 7))
      (bind 'e_center 4)
      (bind 'e_left 3)
         (bind 'e_right 5))))

;s1.2

(define-extended-language L+G L
  [G ** (x : t G)])

(define-judgment-form
  L+G
  #:mode (types I I O)
  #:contract (types G e t)

  [(types G e_1 (-> t_2 t_3))
   (types G e_2 t_2)
   --------------------------
   (types G (e_1 e_2) t_3)]
  
  [(types (x : t_1 G) e t_2)
   -----------------------------------------
   (types G (lambda (x t_1) e) (-> t_1 t_2))]

  [(types G e (-> (-> t_1 t_2) (-> t_1 t_2)))
   ------------------------------------------
   (types G (fix e) (-> t_1 t_2))]
  
  [---------------------
   (types (x : t G) x t)]

  [(types G x_1 t_1)
  #; (side-condition (different x_1 x_2))
   ------------------------------------
   (types (x_2 : t_2 G) x_1 t_1)]
  
  [(types G e num) ...
   -----------------------
   (types G (+ e ...) num)]
  
  [--------------------
   (types G number num)]

  [(types G e_1 num)
   (types G e_2 t)
   (types G e_3 t)
   -----------------------------
   (types G (if0 e_1 e_2 e_3) t)]

  [(types G e num) ...
   -------------------------
   (types G (amb e ...) num)])



(define-language Lambda
  (e ::= x
         (lambda (x ...) e)
	 (e e ...))
  (x ::= variable-not-otherwise-mentioned))

(define-language nat
  [N ::= Zero
         (Plus1 N)])
