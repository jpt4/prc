;;semsim.scm
;;jpt4
;;Reference implementation and simulator of PRC Universal Cell semantics.
;;UTC20160308
;;Guile Scheme v2.0+

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
(define (cell-meta-data cid cls)
  (list-head (cell-list-ref cls cid) 2))
(define (cell-list-head cls cid)
  (list-head cls (if (equal? cid 'p) 1 (+ 1 cid))))
(define (cell-list-tail cls cid)
  (list-tail cls (if (equal? cid 'p) 1 (+ 1 cid))))
(define (pair a b) (list a b))
(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))
;;f((a ...) (b ...)) = (((a b) (a ...)) (... b) (... ...))
(define (cartesian-product lsa lsb)
  (map (lambda (a) (map (lambda (b) (list a b)) lsb)) lsa))

(define (cartesian-product* lsa . lsb)
  (map (lambda (a) (

(define (xor a . b) 
  (cond
   [(and (null? a) (null? b)) #f]
   [(null? b) a]
   [(equal? a (car b)) (if (null? (cdr b)) #f (xor (car b) (cadr b)))]
   [else (car b)]))
     
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
(define (foldr kons knil lst)
  (if (null? lst)
      knil
      (foldr kons (kons (car lst) knil) (cdr lst))))
;; Same as R5RS's reverse
(define (reverse lst)
  (foldr cons '() lst))
(define (reverse-flatten-into x lst)
  (if (pair? x)
      (foldr reverse-flatten-into lst x)
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
(define (cell-peek-state sta elm)
  (if (eq? elm 'state)
      sta
      (let ([splice (list-index uc-core-prototype elm)])
        (list-ref sta splice))))
(define (cell-poke-state ost elm nst) 
  (if (eq? elm 'state) 
      nst
      (let ([splice (list-index uc-core-prototype elm)])
        (append (list-head ost splice)
                (list nst)
                (list-tail ost (+ 1 splice))))))
;old cell list, cell id, replacement cell value
(define (cell-list-poke-state ols cid ncl)
  (list (cell-list-head ols cid) 
        (cons (cell-meta-data cid ols) (list ncl))
        (cell-list-tail ols (+ 1 cid))))

(define (cell-input cell) 
  (list (cell-peek-state cell 'ai) 
        (cell-peek-state cell 'bi) 
        (cell-peek-state cell 'ci)))
;;original cell, list of (state value) pairs to poke
(define (cell-multi-poke ocl pls)
  (foldr (lambda (poke cell) (cell-poke-state cell (car poke) (cadr poke)))
         ocl pls))
;;original cell list, list of (id new-entry) pairs to poke
(define (cell-list-multi-poke ols pls)
  (foldr 
   (lambda (poke clst) (cell-list-poke-state clst (car poke) (cadr poke)))
   ols pls))
        
(define (nbra-id id cls) (list-ref (cadr (cell-list-ref cls id)) 0))
(define (nbrb-id id cls) (list-ref (cadr (cell-list-ref cls id)) 1))
(define (nbrc-id id cls) (list-ref (cadr (cell-list-ref cls id)) 2))
(define (nbra-cell id cls) (cell-list-ref cls (nbra-id id cls)))
(define (nbrb-cell id cls) (cell-list-ref cls (nbrb-id id cls)))
(define (nbrc-cell id cls) (cell-list-ref cls (nbrc-id id cls))) 

;;input classification predicates
(define (zeroed? in) (foldr (lambda (e k) (and (zero? e) k)) #t in))
(define (standard? in)
  (and (not (zeroed? in)) 
       (foldr (lambda (e k) (and (or (zero? e) (equal? e 1)) k)) #t in)))
(define (special? in)
  (map (lambda (e) (append e '(0))) (unpack (cartesian-product special-messages '(0)) '()))
  (xor 
   (equal? (list (car (member (car in) special-messages)) 0 0) in)
   (equal? (list 0 (car (member (cadr in) special-messages)) 0) in)
   (equal? (list 0 0 (car (member (caddr in) special-messages))) in)))
(define (bad? in) (not (or (standard? in) (special? in))))

(define (end-cell-update cid cls) cls)

(define (check-input id cls)
  (let* ([cell (cell-list-ref cls id)]
         [input (list (cell-peek-state cell 'ai) (cell-peek-state cell 'bi) 
                      (cell-peek-state cell 'ci))])
    (if (zeroed? input)
        (pull-for-input id cls)
        (check-role id cls))))

(define (check-role id cls)
  (let* ([cell (cell-list-ref cls id)]
         [role (cell-peek-state cell 'rol)])
    (cond
     [(or (equal? 'proc role) (equal? 'wire role)) (classify-input id cls)]
     [(equal? 'stem role) (stem-process-input)])))

(define (pull-for-input cid cls)
  (let* ([cell (cell-list-ref cls cid)]
         [nbra-ao (cell-peek-state (nbra-cell cid cls) 'ao)] 
         [nbrb-bo (cell-peek-state (nbrb-cell cid cls) 'bo)]
         [nbrc-co (cell-peek-state (nbrc-cell cid cls) 'co)]
         [input (list nbra-ao nbrb-bo nbrc-co)])
    (cond
     [(zeroed? input) (end-cell-update cid cls)]
     [(not (zeroed? input)) 
      (let* ([new-cl 
              (cell-multi-poke cell (list (pair 'ai nbra-ao)
                                          (pair 'bi nbrb-bo)
                                          (pair 'ci nbrc-co)))]
             [new-cls 
              (cell-list-multi-poke 
               cls 
               (list (pair cid new-cl)
                     (pair (nbra-id cid cls) 
                           (cell-poke-state (nbra-cell cid cls) 'ao 0))
                     (pair (nbrb-id cid cls) 
                           (cell-poke-state (nbrb-cell cid cls) 'bo 0))
                     (pair (nbrc-id cid cls) 
                           (cell-poke-state (nbrc-cell cid cls) 'co 0))))])
        (check-role cid new-cls))])))

(define (classify-input cid cls)
  (let* ([cell (cell-list-ref cid cls)]
         [input (cell-input cell)])
    (cond
     [(standard? input) (process-standard-signal cid cls)]
     [(special? input) (process-special-message cid cls)]
     [(bad? input) (process-bad-input cid cls)])))

(define (switch-mem memval) (abs (- memval 1)))

(define (process-standard-signal cid cls)
  (let* ([cell (cell-list-ref cls cid)]
         [input (cell-input cell)]
         [new-wire-cell (cell-multi-poke cell (list (pair 'ao (car input))
                                 (pair 'ai 0)
                                 (pair 'bo (cadr input))
                                 (pair 'bi 0)
                                 (pair 'co (caddr input))
                                 (pair 'ci 0)))]
         [new-cls (cell-list-poke-state 
      cls cid 
      (case (cell-peek-state cell 'rol)
        [(wire) new-wire-cell]
        [(proc) (cell-poke-state new-wire-cell 'mem 
                                 (switch-mem 
                                  (cell-peek-state new-wire-cell 'mem)))]))])
    (end-cell-update cid new-cls)))

#|  
(define (process-special-message cid cls)
  (let* ([cell (cell-list-ref cls cid)]
         [input (cell-input cell)])
    (case 
  |#       
         

;;;universal cell core
(define (uc-core rol mem ai bi ci ao bo co hig buf)
  (define state (mk-uc-core rol mem ai bi ci ao bo co hig buf))
  (define (obj-peek-state elm) (cell-peek-state state elm))
  (define (obj-poke-state elm nst) 
    (set! state (cell-poke-state state elm nst)))
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
;;functional
;;object oriented
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
 
