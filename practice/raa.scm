;raa.scm
;reconfigurable asynchronous rlem cellular automaton
;jpt4
;UTC20160129
;Guile Scheme v2.2

;;import record types
(use-modules (srfi srfi-9))

;;cell state
(define-record-type <rlem-3453-state>
  (rlem-3453-state id role memory a-in b-in c-in a-out b-out c-out buffer
                   high-rail low-rail neighbor-a neighbor-b neighbor-c)
  rlem-3453-state?
  (id id id!) (role rol rol!) (memory mem mem!) 
  (a-in ai ai!) (b-in bi bi!) (c-in ci ci!)
  (a-out ao ao!) (b-out bo bo!) (c-out co co!)
	(buffer buf buf!) (high-rail hig hig!) (low-rail low low!)
  (neighbor-a nba nba!) (neighbor-b nbb nbb!) (neighbor-c nbc nbc!)
	)

(define (cell-list-ref cell-list index) 
	(if (equal? index 'p) 
			(car cell-list)
			(list-ref (cdr cell-list) index)))

;;directions
(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))

;;XX STILL OLD UPDATE
(define (update-cell cell-index cell-list)
	(let* ([cell (cell-list-ref cell-list cell-index)]
				 [in (lambda () (list (ai cell) (bi cell) (ci cell)))] ;lazy in
				 [out (list (ao cell) (bo cell) (co cell))]           
				 [nbra (cell-list-ref cell-list (nba cell))] 
				 [nbrb (cell-list-ref cell-list (nbb cell))] 
				 [nbrc (cell-list-ref cell-list (nbc cell))]
				 [empty? (lambda (c) (equal? '(0 0 0) c))]
				 [inhale ;gather new input from neighbors' outputs
					(lambda () 
						(begin
							(ai! cell (ao nbra)) (bi! cell (bo nbrb)) (ci! cell (co nbrc))
							(ao! nbra 0) (bo! nbrb 0) (co! nbrc 0)))] ;clear nbr outputs
				 [logic-process 
					(lambda ()
						(if (not (empty? (in))) ;did (inhale) acquire new input?
								(begin
									(case (mem cell) ;rotate input right/left, write to output
										['0 (ao! cell (ci cell)) (bo! cell (ai cell)) ;right
												(co! cell (bi cell))]
										['1 (ao! cell (bi cell)) (bo! cell (ci cell)) ;left
												(co! cell (ai cell))])
									(ai! cell 0) (bi! cell 0) (ci! cell 0) ;clear inputs
									(if (equal? (id nba) 'p) 
											(ao! cell 0))        ;}if cell borders the perimeter
									(if (equal? (id nbb) 'p) ;}immediately dispose of its
											(bo! cell 0))        ;}output channel values
									(if (equal? (id nbc) 'p) 
											(co! cell 0))
									)))]
				 [mem-switch (lambda () (mem! cell (abs (- (mem cell) 1))))]) 
		(case (rol cell)
			['proc
			 (cond
				[(and (empty? out) (empty? (in))) (begin (inhale) (logic-process)
																								 (mem-switch))]
				[(and (empty? out) (not (empty? (in)))) (begin (logic-process) 
																											 (mem-switch))]
				[(and (not (empty? out)) (empty? (in))) (inhale)])
			 ]
			['wire
			 (cond
				[(and (empty? out) (empty? (in))) (begin (inhale) (logic-process))]
				[(and (empty? out) (not (empty? (in)))) (logic-process)]
				[(and (not (empty? out)) (empty? (in))) (inhale)])
			 ]
			['stem 'stem]
			 )
		cell))

;;cells do not exist outside of a grid
(define (rlem-hex-grid rows cols)
  (let* ([nbrls (hex-neighbor-list rows cols)]
         [p (rlem-3453-state 'p '_ '_ 0 0 0 0 0 0 '() '_ '_ '_ '_ '_ )]
         [base (cons p (map (lambda (t) (rlem-3453-state 
																				 t 'wire 0 0 0 0 0 0 0 0 0 0 0 0 0))
														(iota (* rows cols))))])
		(map (lambda (e)
					 (nba! (cell-list-ref base (car e)) (cadr e))
					 (nbb! (cell-list-ref base (car e)) (caddr e))
					 (nbc! (cell-list-ref base (car e)) (cadddr e))
					 )
				 nbrls)
		base))

(define (hex-neighbor-list rows cols)
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

;f((a ...) (b ...)) = (((a b) (a ...)) (... b) (... ...))
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

;; Helper function
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

;;XX COLUMN RESET BUG WHEN INCREMENTING ROW
(define (obsolete-hex-neighbor-list rows cols)
	(let ([size (* rows cols)])
		(let next ([i 0] [r 0] [c 0] [nls '()])
      (if (equal? i size) nls ;all points en-neighbored?
					(let ([new-n 
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
												 (south-west-index i))])])
            (if (<= c cols) ;not end of row?
                (next (+ i 1) r (+ c 1) (append nls (list new-n))) ;next col
                (next (+ i 1) (+ r 1) 0 (append nls (list new-n)))) ;next row
						)))))

