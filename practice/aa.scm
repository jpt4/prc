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
	(define perimeter-cell  ;default perimeter trivial cell				 
		(rlem-3453-state 'p 0 0 0 0 0 0 0 '_ '_ '_))
	(define cell-list (if (null? cl-list) (rlem-hex-grid r c) (car cl-list)))
	(define activation-order (random-permutation 
														(list-head cell-list (- (length cell-list) 1))))
	(define (cell-mem! index value)
		((list-ref cell-list index) (list 'mem! value))) 
	(define (cell-in! index channel value)
		(let ([op (case channel ['a ai!] ['b bi!] ['c ci!])])
			(op (list-ref cell-list index) value)
			))			

	(define (self msg)
		(case (car msg)
			['cell-list cell-list]
			['activation-order activation-order]
			['reset-activation-order (set! activation-order
																		 (random-permutation activation-order))]
			['cell-state (list-ref cell-list (cadr msg))]
			['cell-mem! (cell-mem! (cadr msg) (caddr msg))]
			['cell-in! (cell-in! (cadr msg) (caddr msg) (cadddr msg))]
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
			(if (equal? i size) (append base (list p)) ;all points en-neighbored?
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
(define tstrlem0 (rlem-3453-state 'tst 0 0 0 0 0 0 0 0 0 0))
(define tstgrid0 (rlem-hex-grid 5 4))
(define tstlat0 (rlem-3453-lattice 5 4))
(define (tests)
;	(dispnl* 
;						(tstrlem0 '(state))
(begin
		(dispnl* 'cell-list)
		(dispnl* 'cell-tst (dispnl* (tstlat0 '(cell-list))))
		(dispnl* 'activation-order)
		(dispnl* (tstlat0 '(activation-order)))
		(dispnl* 'reset-activation-order)
		(tstlat0 '(reset-activation-order))
		(dispnl* 'activation-order)
		(dispnl* (tstlat0 '(activation-order)))
		
	 )
	)
