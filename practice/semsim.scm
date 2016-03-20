;;semsim.scm
;;jpt4
;;Reference implementation and simulator of PRC Universal Cell semantics.
;;UTC20160308
;;Guile Scheme v2.2

;;;constants
(define standard-signal 1)
(define max-buffer-length 5)
(define special-messages 
  '(stem-init wire-r-init wire-l-init proc-r-init proc-l-init write-buf-zero
              write-buf-one))
(define uc-core-prototype (list 'rol 'mem 'ai 'bi 'ci 'ao 'bo 'co 'hig 'buf))

;;;utilities
(define (cell-list-ref cell-list index) 
  (if (equal? index 'p) 
      (car cell-list)
      (list-ref (cdr cell-list) index)))
(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))
;;f((a ...) (b ...)) = (((a b) (a ...)) (... b) (... ...))
(define (cartesian-product lsa lsb)
  (map (lambda (a) (map (lambda (b) (list a b)) lsb)) lsa))

(define (unpack ls acc)
  (cond
   [(null? ls) (reverse acc)]
   [(and (pair? (car ls)) (not (null? (cdar ls))))
    (unpack (cons (cdar ls) (cdr ls)) (cons (caar ls) acc))]
   [(and (pair? (car ls)) (null? (cdar ls)))
    (unpack (cdr ls) (cons (caar ls) acc))]
   [else (cons (car ls) (unpack (cdr ls) acc))]))
;;source: http://stackoverflow.com/questions/7313563/flatten-a-list-using-only-the-forms-in-the-little-schemer
;; Similar to SRFI 1's fold
(define (fold1 kons knil lst)
  (if (null? lst)
      knil
      (fold1 kons (kons (car lst) knil) (cdr lst))))
;; Same as R5RS's reverse
(define (reverse lst)
  (fold1 cons '() lst))
(define (reverse-flatten-into x lst)
  (if (pair? x)
      (fold1 reverse-flatten-into lst x)
      (cons x lst)))

(define (deep-flatten lst)
    (reverse (reverse-flatten-into lst '())))

#|sophisticated flatten: 
unpack n layers deep, * if unspecified
do/not preserve unpacked '() elements <-- via preprocess tagging?          
|#

(define (zip lsa lsb)
  (map (lambda (e) (list (list-ref lsa e)
                         (list-ref lsb e)))
       (iota (length lsa))))

;;directions
(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

;;;hex-grid
(define (mk-hex-node id neighbor-list cell)
  (list id (cdr (assq id neighbor-list)) cell))
(define (mk-hex-neighbor-list rows cols)
  (let ([grid (unpack (cartesian-product (iota rows) (iota cols)) '())])
    (map
     (lambda (e)
       (let* ([r (car e)] [c (cadr e)] [i (+ (* r cols) c)])
         (cond
          [(even? c) ;point in even column?
           (list i 
                 (if (zero? c) 'p (west-index i)) ;left column?
                 (if (or (zero? r) (equal? c (- cols 1))) ;bottom row/ 
                     'p                                  ;right column?
                     (south-east-index i cols))
                 (if (equal? c (- cols 1)) ;right-most column?
                     'p 
                     (north-east-index i)))]
          [(odd? c) ;point in odd column?
           (list i 
                 (if (equal? c (- cols 1)) ;right-most column?
                     'p
                     (east-index i))
                 (if (equal? r (- rows 1)) ;upper row?
                     'p
                     (north-west-index i cols))
                 (south-west-index i))])))
     grid)))
(define (mk-hex-grid rows cols default-cell perimeter-cell)
  (let* ([nbrls (mk-hex-neighbor-list rows cols)]
         [p (list 'p '(_ _ _) perimeter-cell)]
         [base (cons p (map (lambda (i) (mk-hex-node i nbrls default-cell))
                            (iota (* rows cols))))])
    base))

;;;functional
(define (mk-uc-core rol mem ai bi ci ao bo co hig buf)
  (list rol mem ai bi ci ao bo co hig buf))
(define (peek-state sta elm)
  (if (eq? elm 'state)
      sta
      (let ([splice (list-index uc-core-prototype elm)])
        (list-ref sta splice))))
(define (poke-state ost elm nst) 
  (if (eq? elm 'state) 
      nst
      (let ([splice (list-index uc-core-prototype elm)])
        (append (list-head ost splice)
                (list nst)
                (list-tail ost (+ 1 splice))))))
(define (nbra-id id cls)
  (list-ref (cadr (cell-list-ref cls id)) 0))
(define (nbrb-id id cls)
  (list-ref (cadr (cell-list-ref cls id)) 1))
(define (nbrc-id id cls)
  (list-ref (cadr (cell-list-ref cls id)) 2))

(define (check-input id cls)
  (let* ([cell (cell-list-ref cls id)]
         [input (list (peek-state cell 'ai) (peek-state cell 'bi) 
                      (peek-state cell 'ci))])
    (if (equal? '(0 0 0) input)
        (pull-for-input id cls)
        (check-role id cls))))

(define (check-role id cls)
  (let* ([cell (cell-list-ref cls id)]
         [role (peek-state cell 'rol)])
    (cond
     [(or (equal? 'proc role) (equal? 'wire role)) (classify-input id cls)]
     [(equal? 'stem role) (stem-process-input)])))

(define (pull-for-input id cls)
  (let* ([cell (cell-list-ref cls id)]
         [nbra (cell-list-ref cls (nbra-id id cls))] 
         [nbrb (cell-list-ref cls (nbrb-id id cls))]
         [nbrc (cell-list-ref cls (nbrc-id id cls))]
         [nbra-ao (peek-state nbra 'ao)]
         [nbrb-bo (peek-state nbrb 'bo)]
         [nbrc-co (peek-state nbrc 'co)]
         [input (list nbra-ao nbrb-bo nbrc-co)])
    
    



#|
(define (process-standard-signal id cls)
  (let* ([nba (cell-list-ref cls (nbra-id id cls))]
         [nbb (cell-list-ref cls (nbrb-id id cls))]
         [nbc (cell-list-ref cls (nbrc-id id cls))])
  |#  
    

;;;universal cell core
(define (uc-core rol mem ai bi ci ao bo co hig buf)
  (define state (mk-uc-core rol mem ai bi ci ao bo co hig buf))
  (define (obj-peek-state elm) (peek-state state elm))
  (define (obj-poke-state elm nst) (set! state (poke-state state elm nst)))
  (define (self msg . arg)
    (case msg
      [(?) (obj-peek-state (car arg))]
      [(!) (obj-poke-state (car arg) (cadr arg))]
      [else (list msg arg)]
      ))
  self
)

;;;universal cell with rlem 3-453 behavior
(define (uc-3453 rol mem ai bi ci ao bo co hig buf)
  (define cell-core (uc-core rol mem ai bi ci ao bo co hig buf))
  (define (self msg . arg)
    (cond
     [(member msg '(! ?)) (cell-core (list msg arg))]
     [else (list msg arg)]
     ))
  self
  )

;;;lattice
(define (hex-lattice rows cols default perimeter)
  (define grid (mk-hex-grid rows cols default perimeter))
  (define (self msg . arg)
    (case msg
      [(print) 
       (dispnl* (map (lambda (c) 
                       (list (car c) (cadr c) ((caddr c) '? 'state))) grid))]
      [else (list msg arg)]
      ))
  self
  )

;;;defaults
(define default-core-obj (uc-core 'stem 0 0 0 0 0 0 0 '(_ _ _) '(_ _ _ _ _)))
(define default-core-perim-obj 
  (uc-core 'perim 0 0 0 0 0 0 0 '(_ _ _) '(_ _ _ _ _)))
(define default-core-perim-fun 
  (mk-uc-core 'perim 0 0 0 0 0 0 0 '(_ _ _) '(_ _ _ _ _)))
(define default-core-fun 
  (mk-uc-core 'stem 0 0 0 0 0 0 0 '(_ _ _) '(_ _ _ _ _)))
(define default-lat-fun (mk-hex-grid 4 5 default-core-fun default-core-perim-fun))
(define default-lat-obj (hex-lattice 4 5 default-core-obj default-core-perim-obj))
;;;test suite
(define tst-core-obj default-core-obj)
 
