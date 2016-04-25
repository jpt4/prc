;;fsmsim.scm
;;jpt4
;;PRC Universal Cell semantics implemented via Finite State Machines, with
;;the goal of provable correctness.
;;UTC20160410
;;Guile Scheme v2.0+

;;;;universal cell
;;;constants
(define standard-signal 1)
(define max-buffer-length 5)
(define special-messages 
  '(stem-init wire-r-init wire-l-init proc-r-init proc-l-init write-buf-zero
              write-buf-one))
(define universal-cell-node-prototype
  (list 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co))
(define universal-cell-fsm-prototype
  (list 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co 'nao 'nbo 'nco))

;;;constructors
(define (mk-uc-cell r m h b s ai bi ci ao bo co)
  (list r m h b s ai bi ci ao bo co))
(define (mk-uc-fsm cell-index cell-list)
  (let* ([cell (cell-list-ref cell-index cell-list)]
         [nao (ao (nbra-cell cell-index cell-list))]
         [nbo (bo (nbrb-cell cell-index cell-list))]
         [nco (co (nbrc-cell cell-index cell-list))])
    (append cell (list nao nbo nco))))

;;;get primitives
(define (peek cell field . s)
  (if (null? s)
      (list-ref cell (list-index universal-cell-fsm-prototype field))
      (peek* cell (cons field s))))
(define (peek* cell fields)
  (if (pair? fields)
      (map (lambda (e) 
             (list-ref cell (list-index universal-cell-fsm-prototype e)))
           fields)
      (peek cell fields)))
;;;set primitives
(define (poke cell field val)
  (let ([pivot (list-index universal-cell-fsm-prototype field)])
    (append (list-head cell pivot) (list val) (list-tail cell (+ 1 pivot)))))
(define (poke* cell fvls)
  (if (eq? (length fvls) 1)
      (poke cell (car fvls) (cadr fvls))
      (map (lambda (e) (if (eq? (car e) (car fvls)) (cadr fvls) (cadr e)))
           (zip universal-cell-fsm-prototype cell))))      
;;common access patterns
(define (rol cell) (peek cell 'rol)) (define (mem cell) (peek cell 'mem))
(define (hig cell) (peek cell 'hig)) (define (buf cell) (peek cell 'buf))
(define (smb cell) (peek cell 'smb)) (define (ai cell) (peek cell 'ai))
(define (bi cell) (peek cell 'bi)) (define (ci cell) (peek cell 'ci))
(define (ao cell) (peek cell 'ao)) (define (bo cell) (peek cell 'bo))
(define (co cell) (peek cell 'co)) (define (nao cell) (peek cell 'nao))
(define (nbo cell) (peek cell 'nbo)) (define (nco cell) (peek cell 'nco))

(define (clear-input cell) (poke* cell '((ai 0) (bi 0) (ci 0))))
(define (get-input cell) 
  (list (peek 'ai cell) (peek 'bi cell) (peek 'ci cell)))  
(define (right-rotate-input cell)
  (let* ([ai (peek cell 'ai)] [bi (peek cell 'bi)] [ci (peek cell 'ci)])
    (poke* cell (list (pair 'ao ci) (pair 'bo ai) (pair 'co 'bi)))))
(define (left-rotate-input cell)
  (let* ([ai (peek cell 'ai)] [bi (peek cell 'bi)] [ci (peek cell 'ci)])
    (poke* cell (list (pair 'ao bi) (pair 'bo ci) (pair 'co 'ai)))))
(define (switch-mem cell)
  (poke cell 'mem (abs (- (peek cell 'mem) 1))))

;;;input predicates
(define (clear? ls) (and-map zero? ls))
(define (present? ls) (and (not (empty? ls)) (not (clear? ls))))
(define empty? null?)
(define (special? mls) 
  (cond
   [(eq? (length mls) 3)
    (let* ([lsa (cartesian-power special-messages '(0) '(0))]
           [lsb (map (lambda (a) (list-rotate a 1)) lsa)] 
           [lsc (map (lambda (b) (list-rotate b 1)) lsb)] 
           [lst (cons lsa (cons lsb lsc))])
      (exactly? 1 (lambda (l) (member mls l)) lst))]
   [(not (pair? mls)) (member mls special-messages)]))
(define (standard? sls)
  (let ([lss (rember '(0 0 0) (cartesian-power '(1 0) '(1 0) '(1 0)))])
    (exactly? 1 (lambda (l) (eq? sls l)) lss)))
(define (bad? bls) 
  (not (exactly? 1 true? `(,(standard? bls) ,(special? bls)))))
    
;;;utilities
(define (cartesian-product lsa lsb)
  (map (lambda (a) (map (lambda (b) (list a b)) lsb)) lsa))
(define (cartesian-power lsa . lsb)
  (map deep-flatten 
       (fold-left (lambda (a b) (unpack (cartesian-product a b))) lsa lsb)))

;;deep-flatten
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

(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))
(define (exactly? num fn ls)
  (let ([fil (filter fn ls)])
    (if (eq? (length fil) num)
        fil
        #f)))
(define (foldr op base ls)
  (if (null? ls)
      base
      (op (car ls) (fold-right op base (cdr ls)))))
(define (foldl op base ls)
  (if (null? ls)
      base
      (fold-left op (op base (car ls)) (cdr ls))))
(define (list-bin->dec bls)
  (cadr 
   (foldl (lambda (acc new)
            (list (+ (car acc) 1) (+ (* (expt 2 (car acc)) new) (cadr acc))))
          '(0 0) (reverse bls))))
(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))
(define (pair a b) (list a b))
(define (rember val ls) 
  (foldl (lambda (acc i) (if (eq? i val) acc (cons i acc))) '() ls))
(define (true? t) (eq? #t t))
(define (unpack ls) (unpack-aux ls '()))  
(define (unpack-aux ls acc)
  (cond
   [(null? ls) (reverse acc)]
   [(and (pair? (car ls)) (not (null? (cdar ls))))
    (unpack-aux (cons (cdar ls) (cdr ls)) (cons (caar ls) acc))]
   [(and (pair? (car ls)) (null? (cdar ls)))
    (unpack-aux (cdr ls) (cons (caar ls) acc))]
   [else (cons (car ls) (unpack-aux (cdr ls) acc))]))
(define (zip lsa lsb)
  (map (lambda (e) (list (list-ref lsa e)
                         (list-ref lsb e)))
       (iota (length lsa))))

;;;cell->(next-state cell) functions
(define (activate cell) (cons 'check-mail cell))
;;SMB can apply regardless of output status; mail is non-blocking input
(define (check-mail cell)
  (let ([mail (peek 'smb cell)])
    (cond
     [(empty? mail) (cons 'check-input cell)]
     [(special? mail) (cons 'process-special-message cell)])))

(define (check-input cell)
  (let ([input (get-input cell)])
    (cond
     [(clear? input) (cons 'collect-input cell)]
     [(present? input) (cons 'classify-input cell)])))

(define (collect-input cell)
  (let* ([new-input (list (peek 'nao cell) (peek 'nbo cell) (peek 'nco cell))]
         [new-cell (poke* cell '((nao 0) (nbo 0) (nco 0)))])
    (cond
     [(clear? new-input) (cons 'end-activation new-cell)]
     [(present? new-input) (cons 'classify-input new-cell)])))
                         
;;we have some actionable non-smb input
(define (classify-input cell)
  (let ([input (get-input cell)])
    (cond
     [(special? input) 
      (cons 'process-special-message cell)] ;output irrelevant
     [(standard? input) (cons 'check-output cell)]
     [(bad? input) (cons 'end-activation (clear-input cell))])))

(define (check-output cell)
  (let ([output (list (peek 'ao cell) (peek 'bo cell) (peek 'co cell))])
    (cond
     [(clear? output) (cons 'process-standard-signal cell)] 
     [(present? output) ;standard input blocks if output present
      (cons 'end-activation cell)])))

(define (process-standard-signal cell)
  (let* ([rol (peek cell 'rol)]
         [input (get-input cell)])
    (case rol
     [(stem) (cons 'stem-process-standard cell)]
     [(wire) (cons 'wire-process-standard cell)]
     [(proc) (cons 'proc-process-standard cell)])))

(define (wire-process-standard cell)
  (cons 'end-activation 
        (clear-input (case (peek cell 'mem)
                       [(0) (right-rotate-input cell)]
                       [(1) (left-rotate-input cell)]))))

(define (proc-process-standard cell)
  (cons 'end-activation (switch-mem (wire-process-standard cell))))

(define (stem-process-standard cell) 
  (let* ([input (get-input cell)] [hig (peek cell 'hig)] 
         [buf (peek cell 'buf)])
    (cond
     [(empty? hig) (cons 'end-activation (poke cell 'hig get-input))]
     [(and (>= (length buf) 0) (<= (length buf) 3))
      (cons 'end-activation 
            (clear-input (poke cell 'buf (cons buf (list 
                                                    (case (eq? input hig)
                                                      [(#t) 1] [(#f) 0]))))))]
     [(eq? (length buf) (- max-buffer-length 1))
      (cons 'stem-process-buffer 
            (clear-input (poke cell 'buf (cons buf (list 
                                                    (case (eq? input hig)
                                                      [(#t) 1] [(#f) 0]))))))]
     )))
(define (stem-process-buffer cell)
  (let* ([buf (peek cell 'buf)] [tar (list-head 2 buf)] 
         [msg (list-ref (list-bin->dec (cddr buf)) special-messages)])
    (cons 'end-activaion
          (poke* cell `((buf '()) ,(list (cond 
                                          [(eq? '(0 0) tar) 'smb]
                                          [(eq? '(0 1) tar) 'ai]
                                          [(eq? '(1 0) tar) 'bi]
                                          [(eq? '(1 1) tar) 'ci])
                                         msg))))))

(define (process-special-message cell)
  (let ([msg (if (null? (peek cell 'smb))
                   (car (filter (lambda (i) (member i special-messages)) 
                                (get-input cell)))
                   (peek cell 'smb))])
    (cons 'end-activation (primitive-eval `(,msg ,cell)))))

(define (stem-init cell)
  (poke* cell '((rol stem) (mem 0) (hig '()) (buf '()) (smb '()) 
                (ai 0) (bi 0) (ci 0) (ao 0) (bo 0) (co 0))))
(define (wire-r-init cell)
  (poke* cell '((rol wire) (mem 0) (hig '()) (buf '()) (smb '()) 
                (ai 0) (bi 0) (ci 0) (ao 0) (bo 0) (co 0))))
(define (wire-l-init cell)
  (poke* cell '((rol wire) (mem 1) (hig '()) (buf '()) (smb '()) 
                (ai 0) (bi 0) (ci 0) (ao 0) (bo 0) (co 0))))
(define (proc-r-init cell)
  (poke* cell '((rol proc) (mem 0) (hig '()) (buf '()) (smb '()) 
                (ai 0) (bi 0) (ci 0) (ao 0) (bo 0) (co 0))))
(define (proc-l-init cell)
  (poke* cell '((rol proc) (mem 1) (hig '()) (buf '()) (smb '()) 
                (ai 0) (bi 0) (ci 0) (ao 0) (bo 0) (co 0))))
(define (write-buf-zero cell)
  (poke* cell `((buf ,(append (peek cell 'buf) (list 0))) (smb '())
                (ai 0) (bi 0) (ci 0))))
(define (write-buf-one cell)
  (poke* cell `((buf ,(append (peek cell 'buf) (list 1))) (smb '())
                (ai 0) (bi 0) (ci 0))))

;;;universal cell matrix
;;;cell list
(define (cell-list-ref cell-list index) 
  (if (equal? index 'p) 
      (car cell-list)
      (list-ref (cdr cell-list) index)))
(define (cell-list-head cls cid)
  (list-head cls (if (equal? cid 'p) 1 (+ 1 cid))))
(define (cell-list-tail cls cid)
  (list-tail cls (if (equal? cid 'p) 1 (+ 1 cid))))
(define (nbra-id cid cls) (list-ref (cadr (cell-list-ref cls cid)) 0))
(define (nbrb-id cid cls) (list-ref (cadr (cell-list-ref cls cid)) 1))
(define (nbrc-id cid cls) (list-ref (cadr (cell-list-ref cls cid)) 2))
(define (nbra-cell cid cls) (cell-list-ref cls (nbra-id cid cls)))
(define (nbrb-cell cid cls) (cell-list-ref cls (nbrb-id cid cls)))
(define (nbrc-cell cid cls) (cell-list-ref cls (nbrc-id cid cls))) 

;;directions
(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

;;hex-grid
(define (mk-hex-node cid neighbor-list cell)
  (list cid (cdr (assq cid neighbor-list)) cell))
(define (mk-hex-neighbor-list rows cols)
  (let ([grid (unpack (cartesian-product (iota rows) (iota cols)))])
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
;;toroidal hex grid
(define (mk-wrap-hex-grid rows cols default-cell)
  (let* ([nbrls (mk-wrap-hex-neighbor-list rows cols)]
         [p (list 'p '(_ _ _) 'perimeter-cell)]
         [base (cons p (map (lambda (i) (mk-hex-node i nbrls default-cell))
                            (iota (* rows cols))))])
    base))
