;;prelude.scm
;;jpt4
;;Functions generally useful elsewhere
;;UTC20160403
;;Guile Scheme v2.0+

;;;in lexicographical order

(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))

;;f((a ...) (b ...)) = (((a b) (a ...)) (... b) (... ...))
(define (cartesian-product lsa lsb)
  (map (lambda (a) (map (lambda (b) (list a b)) lsb)) lsa))
(define (cartesian-power lsa . lsb)
  (map deep-flatten 
       (fold-left (lambda (a b) (unpack (cartesian-product a b))) lsa lsb)))

(define (exactly num fn ls)
  (let ([fil (filter fn ls)])
    (if (eq? (length fil) num)
        fil
        #f)))

;;flatten
;;source: http://stackoverflow.com/questions/7313563/flatten-a-list-using-only-the-forms-in-the-little-schemer
;; Similar to SRFI 1's fold
(define (fold kons knil lst)
  (if (null? lst)
      knil
      (fold kons (kons (car lst) knil) (cdr lst))))
(define (reverse-list lst)
  (fold-left (lambda (a b) (cons b a)) '() lst))
(define (reverse-flatten-into x lst)
  (if (pair? x)
      (fold reverse-flatten-into lst x)
      (cons x lst)))
(define (deep-flatten lst)
    (reverse-list (reverse-flatten-into lst '())))

(define (fold-right op base ls)
  (if (null? ls)
      base
      (op (car ls) (fold-right op base (cdr ls)))))
(define (fold-left op base ls)
  (if (null? ls)
      base
      (fold-left op (op base (car ls)) (cdr ls))))

(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))

(define (member* e ls) (filter (lambda (a) (eq? a e)) ls))

(define (pair a b) (list a b))

(define (unpack ls) (unpack-aux ls '()))  
(define (unpack-aux ls acc)
  (cond
   [(null? ls) (reverse acc)]
   [(and (pair? (car ls)) (not (null? (cdar ls))))
    (unpack-aux (cons (cdar ls) (cdr ls)) (cons (caar ls) acc))]
   [(and (pair? (car ls)) (null? (cdar ls)))
    (unpack-aux (cdr ls) (cons (caar ls) acc))]
   [else (cons (car ls) (unpack-aux (cdr ls) acc))]))
;;XXBroken (xor #t #f) => #f
(define (xor a . b) 
  (cond
   [(and (null? a) (null? b)) #f]
   [(null? b) a]
   [(equal? a (car b)) (if (null? (cdr b)) #f (xor (car b) (cadr b)))]
   [else (car b)]))
(define (zip lsa lsb)
  (map (lambda (e) (list (list-ref lsa e)
                         (list-ref lsb e)))
       (iota (length lsa))))
