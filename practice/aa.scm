;aa.scm
;asynchronous rlem cellular automata
;jpt4
;UTC20160121
;Guile v2.2


#|
Three-in/three-out reversible logic elements with memory (RLEMs/rlems) 
tessellate to cover a hexagonal grid. Each RLEM has three neighbors, adjacent
on each of its A, B, and C channels. 

Ex:    \B
        R3-A-
\B     /C
 R0-A-R1
/C     \B
        R2-A-
       /C

Each RLEM executes a reversible automaton operation, moving symbols from input
to output, and changing the value in memory. Of the many three input/output
RLEMs (of the many n input/output RLEMs), 3-453 behaves as follows:

Standard RLEM 3-453:
IN: MEM A B C <-> OUT: MEM A B C
    0/1 0 0 0          0/1 0 0 0
     0  0 0 1           1  1 0 0
     0  0 1 0           1  0 0 1
     0  1 0 0           1  0 1 0

When viewed in diagram form, MEM=0 rotates symbols to the right, while MEM=1
rotates to the left.

Extending this behavior to simultaneous process multiple non-zero input signals
yields the following transition table:

Multi-signal rlem 3453:
IN: MEM A B C <-> OUT: MEM A B C
    0/1 0 0 0          0/1 0 0 0
     0  0 0 1           1  1 0 0  
     0  0 1 0           1  0 0 1    
     0  0 1 1           1  1 0 1    
     0  1 0 0           1  0 1 0    
     0  1 0 1           1  1 1 0    
     0  1 1 0           1  0 1 1
     0  1 1 1           1  1 1 1    

Considered as cells in an asynchronous cellular automaton, each RLEM maintains
a quasi-delay invariant [0] dependence on the receipt of symbols from its 
neighbors by adopting a pull-based state update strategy. In hardware, or truly
concurrent software processes, this might be done by all RLEMs cyclically
polling their neighbors output channels for non-zero values. In the software 
simulated concurrency of this program, every "time step" sees all RLEMs placed 
in a random order and triggered sequentially by the master scheduler. In its
turn, each active RLEM scans the output channels of its neighbors, consumes any
accessible non-zero values, and performs the appropriate state transition.

[0] Robust to timing differences, except insofar as the signal fan-in/fan-out 
interval is assumed constant.

Pull-based RLEM state update:
At every time step T:

MASTER-SCHEDULER(T):
 CELLS={R0, R1, R2, ... Rn} (immutable)
 ACTIVATION-ORDER=RANDOM-PERMUTATION(CELLS)={R4, R2, R9, R16, ... Rk}
 FOR-EACH(ACTIVATION-ORDER):
  INPUT-SYMBOLS={OUTPUT(NEIGHBOR-A), OUTPUT(NEIGHBOR-B), OUTPUT(NEIGHBOR-C)}
  NEXT-STATE=TRANSITION-TABLE(MEMORY, INPUT-SYMBOLS)
 NEXT
MASTER-SCHEDULER(T+1)
|#

;;import record types
(use-modules (srfi srfi-9))

(define-record-type <rlem-3453-state>
	(rlem-3453-state id memory a-in b-in c-in a-out b-out c-out 
									 neighbor-a neighbor-b neighbor-c)
	rlem-3453-state?
	(id id id!)
	(memory mem mem!) 
	(a-in ai ai!) (b-in bi bi!) (c-in ci ci!)
	(a-out ao ao!) (b-out bo bo!) (c-out co co!)
	(neighbor-a nba nba!) (neighbor-b nbb nbb!) (neighbor-c nbc nbc!)
)

;;don't develop rlem object unless rlem record proves insufficient
#;(define (rlem-3453 id mem ai bi ci ao bo co na nb nc)
	(define state (rlem-3453-state id mem ai bi ci ao bo co na nb nc))
	
	(define (self msg)
		(case (car msg)
			['state state]
			)
		)
	self
)

(define (rlem-3453-lattice r c . cl-list)
	(define cell-list (if (null? cl-list) (rlem-hex-grid r c) (car cl-list)))
	(define activation-order (map id (random-permutation (cdr cell-list))))
	(define (reset-activation-order) 
		(set! activation-order (random-permutation activation-order)))
	(define (cell-list-ref index) (if (equal? index 'p) 
																		(car cell-list)
																		(list-ref (cdr cell-list) index)))
	(define (cell-mem! index value)
		(mem! (cell-list-ref index) value))
	(define (cell-in! index channel value)
		(let ([op (case channel ['a ai!] ['b bi!] ['c ci!])])
			(op (cell-list-ref index) value)))
	(define (cell-out! index channel value)
		(let ([op (case channel ['a ao!] ['b bo!] ['c co!])])
			(op (cell-list-ref index) value)))
	(define (update-cell index)
		(let* ([cell (cell-list-ref index)] 
					 [in (lambda () (list (ai cell) (bi cell) (ci cell)))] ;lazy in
					 [out (list (ao cell) (bo cell) (co cell))]           
					 [nba (cell-list-ref (nba cell))] 
					 [nbb (cell-list-ref (nbb cell))]
					 [nbc (cell-list-ref (nbc cell))]
					 [empty? (lambda (c) (equal? '(0 0 0) c))]
					 [inhale ;gather new input from neighbors' outputs
						(lambda () 
							(begin
								(ai! cell (ao nba)) (bi! cell (bo nbb)) (ci! cell (co nbc))
								(ao! nba 0) (bo! nbb 0) (co! nbc 0)))] ;clear nbr outputs
					 [process 
						(lambda ()
							(if (not (empty? (in))) ;did (inhale) acquire new input?
									(begin
										(case (mem cell) ;rotate in right/left, write to out
											['0 (ao! cell (ci cell)) (bo! cell (ai cell)) ;right
													(co! cell (bi cell))]
											['1 (ao! cell (bi cell)) (bo! cell (ci cell)) ;left
													(co! cell (ai cell))])
										(ai! cell 0) (bi! cell 0) (ci! cell 0) ;clear inputs
										(mem! cell (abs (- (mem cell) 1))))))]) ;mem switch
			(cond
			 [(and (empty? out) (empty? (in))) (begin (inhale) (process))]
			 [(and (empty? out) (not (empty? (in)))) (process)]
			 [(and (not (empty? out)) (empty? (in))) (inhale)])
			))
	(define (update-lattice steps)
		(let next ([t 0])
			(if (< t steps)
					(begin 
						(map update-cell activation-order)
						(reset-activation-order)
						(next (+ 1 t))))))
	
	(define (self msg)
		(case (car msg)
			;inspect
			['cell-list cell-list]
			['activation-order activation-order]
			['cell-state (cell-list-ref (cadr msg))]
			;manipulate
			['reset-activation-order (reset-activation-order)]
			['cell-mem! (cell-mem! (cadr msg) (caddr msg))]
			['cell-in! (cell-in! (cadr msg) (caddr msg) (cadddr msg))]
			['cell-out! (cell-out! (cadr msg) (caddr msg) (cadddr msg))]
			;iterate
			['update-cell (update-cell (cadr msg))]
			['update-lattice (update-lattice (cadr msg))]
			))										
	self
)

;;f(rows, cols) => neighbor-list, neighbor-list:={(Id,nba,nbb,nbc)...}
(define (rlem-hex-grid rows cols)
	(let* ([size (* rows cols)]
				 [p	(rlem-3453-state 'p 0 0 0 0 0 0 0 '_ '_ '_)]
				 [base (map (lambda (t) (rlem-3453-state t 0 0 0 0 0 0 0 0 0 0)) 
										(iota size))])
		(let next ([i 0] [r 0] [c 0])
			(if (equal? i size) (cons p base) ;all points en-neighbored?
					(let ([e (list-ref base i)])
						(id! e i)
						(cond
						 [(even? c) ;point in even column?
							(nba! e (if (zero? c) 'p (west-index i))) ;left-most column?
							(nbb! e (if (or (zero? r) (equal? c (- cols 1))) ;bottom row or 
													'p                             ;right-most column?
													(south-east-index i cols)))
							(nbc! e (if (equal? c (- cols 1)) ;right-most column?
													'p 
													(north-east-index i)))]
						 [(odd? c) ;point in odd column?
							(nba! e (if (equal? c (- cols 1)) ;right-most column?
												 'p
												 (east-index i)))
							(nbb! e (if (equal? r (- rows 1)) ;upper row?
												 'p
												 (north-west-index i cols)))
							(nbc! e (south-west-index i))])
						(if (equal? c (- cols 1)) ;end of row?
								(next (+ i 1) (+ r 1) 0)
								(next (+ i 1) r (+ c 1)))
						)))))

(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

;;randomly permute a list
(define (random-permutation list)
	(let tail ([acc '()] [ls list])
		(cond
		 [(null? ls) acc]
		 [else (let ([r (random (length ls))])
						 (tail (cons (list-ref ls r) acc) (remove-list-ref ls r)))])))

;;produce list sans element at index ref
(define (remove-list-ref ls ref)
	(append (list-head ls ref) (list-tail ls (+ ref 1))))

;;utility functions
(define (dispnl* txt . res)
	(cond
	 [(pair? txt)	(begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
	 [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))

(define (alt-dispnl* txt)
	(if (not (null? txt))
			(begin (display (car txt)) (newline) (dispnl* (cdr txt)))))

;;test suite
(define (random-lattice r c)
	(let ([lat (rlem-3453-lattice r c)]
				[flip (lambda () (random 2))])
		(map (lambda (i) 
					 (lat `(cell-mem! ,i ,(flip)))
					 (lat `(cell-in! ,i a ,(flip)))
					 (lat `(cell-in! ,i b ,(flip)))
					 (lat `(cell-in! ,i c ,(flip)))
					 (lat `(cell-out! ,i a ,(flip)))					 
					 (lat `(cell-out! ,i b ,(flip)))					 
					 (lat `(cell-out! ,i c ,(flip))))
				 (lat '(activation-order)))
		lat))

(define (step lat . t)
	(dispnl* (cons 'original-lattice-state (lat '(cell-list))))
	(cond
	 [(null? t)	(step lat 1)]
	 [(equal? (car t) 1) (begin 
												 (lat '(update-lattice 1))
												 (dispnl* (cons `(steps-to-go ,(car t)) 
																				(lat '(cell-list)))))]
	 [else (begin 		 
					 (lat '(update-lattice 1))
					 (dispnl* (cons `(steps-to-go ,(car t)) (lat '(cell-list))))
					 (step lat (- (car t) 1)))]))

(define tstrlem0 (rlem-3453-state 'tst 0 0 0 0 0 0 0 0 0 0))
(define tstgrid0 (rlem-hex-grid 5 4))
(define tstlat0 (rlem-3453-lattice 5 4))
(define randlat0 (random-lattice 5 5))
(define (tests)
	;simple tests
	(dispnl* (cons 'tstlat0-cell-list-new-born (tstlat0 '(cell-list))))
	(dispnl* (list 'activation-order (tstlat0 '(activation-order))))
	(dispnl* (cons 'reset-activation-order (tstlat0 '(reset-activation-order))))
	(dispnl* (list 'activation-order (tstlat0 '(activation-order))))
	;single cell tests
	(dispnl* (cons 'cell-5-pre-ai! (tstlat0 '(cell-state 5))))
	(dispnl* (cons 'cell-in!-5-a-1 (tstlat0 '(cell-in! 5 a 1))))
	(dispnl* (cons 'cell-5-pre-update (tstlat0 '(cell-state 5))))
	(dispnl* (cons 'update-cell-5 (tstlat0 '(update-cell 5))))
	(dispnl* (cons 'cell-5-post-update (tstlat0 '(cell-state 5))))
	(dispnl* (cons 'cell-8-pre-update (tstlat0 '(cell-state 8))))
	(dispnl* (cons 'update-cell-8 (tstlat0 '(update-cell 8))))
	(dispnl* (cons 'cell-8-post-update (tstlat0 '(cell-state 8))))
	;randomized lattice tests
	(dispnl* (cons 'randlat0-cell-list-new-born (randlat0 '(cell-list))))
	(dispnl* (cons 'randlat0-update-lattice-5 (randlat0 '(update-lattice 5))))
	(dispnl* (cons 'randlat0-cell-post-update-lattice-5 (randlat0 '(cell-list))))
)
