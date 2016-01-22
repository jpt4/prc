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

(define (rlem-3453 id mem ai bi ci ao bo co na nb nc)
	(define state (rlem-3453-state id mem ai bi ci ao bo co na nb nc))
	
	(define (self msg)
		'msg)
	self
)

(define (lattice r c . cell-neighbor-list)
	(define neighbor-list (if (null? cell-neighbor-list)
														(hex-grid r c)
														(car cell-neighbor-list)))
	(define activation-order (random-permutation neighbor-list))

	(define (cell-mem! index value)
		(mem! (list-ref neighbor-list index) value))
	(define (cell-in! index channel value)
		(let ([op (case channel ['a ai!] ['b bi!] ['c ci!])])
			(op (list-ref neighbor-list index) value)
			))			

	(define (self msg)
		(case (car msg)
			['neighbor-list neighbor-list]
			['activation-order activation-order]
			['cell-state (list-ref neighbor-list (cadr msg))]
			['cell-mem! (cell-mem! (cadr msg) (caddr msg))]
			['cell-in! (cell-in! (cadr msg) (caddr msg) (cadddr msg))]
			))
										
	self
)

(define (hex-grid rows cols)
	'(rows x cols)
)

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

;;test suite
(define tstrlem0 (rlem-3453 0 0 0 0 0 0 0 0 0 0 0))
(define tstlat0 (lattice 5 4))
(define (tests)
	(tstlat0 '(neighbor-list)))
