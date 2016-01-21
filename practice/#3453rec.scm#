;;3453rec.scm
;;UTC20160115
;;jpt4
;;RLEM-3453 universal cells using record types
;;Guile Scheme v2.2

#|
3-phase async co-ordination method:
{A,B,C} = 0, 1, REQ, ACK

MAKE IT PULL BASED: if output channels are clear, query nbrs 
                    1. after mem change stabilizes
                    2. after querying all nbrs without reply
All output channels open requirement => rlem blocks on one blocked channel

R0
start
if OPEN?(ao bo co)
   then
       QUE(nba, nbb, nbc) -> ai, bi, ci
       NEXT-STATE(ai bi ci mem) -> ao bo co mem
       GOTO(start)
   else
       GOTO(start)

MAKE IT PUSH BASED: pull based requires multiple concurrent sampling or 
simulation thereof, push-based is sequential.

DEPTH-FIRST PROPAGATING UPDATE: Hybrid pull/push - symbol inhalation ends by
messaging neighbors in A,B,C order of status update. A->A->A until a
non-transitionable cell is reached, then message execution backtracks one
level, A->A->B. Defers update order to runtime calling convention. 

        R3
       /C
R1-A-R0
       \B
        R2
R0                      -A-   R1  
MEM AI BI CI AO BO CO         MEM AI BI CI AO BO CO
0   0        1



rlem<cell<?node

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

TODO: Write generic asynchronously updating hex lattice. Specify cell type,
grid dimensions as arguments.
               
|#

(use-modules (srfi srfi-9))

;;independently operative 3453 universal cells
;	(nbr-a nba nba!) (nbr-b nbb nbb!) (nbr-c nbc nbc!) ;_ (default) procedure-id


;;universal cells nodes with rlem-3453 core behavior
;;neighbor relations processed by lattice
(define-record-type <cell-3453-state>
	(cell-3453-state memory a-in b-in c-in a-out b-out c-out
									 role buffer high-rail low-rail)
	cell-3453-state?
	(memory mem	mem!) ;0,r (default) 1,l
	(a-in ai ai!) (b-in bi bi!) (c-in ci ci!)    ;0 (default) 1
	(a-out ao ao!) (b-out bo bo!) (c-out co co!)
	(role rol rol!) ;stem (default) wire proc
	(buffer buf buf!) ;(_ _ _ _ _) (default)
	(high-rail hig hig!) (low-rail low low!) ;_ (default) a b c 
)

(define (next-cell-state cell)
	(if (and-map (lambda (p) (zero? (p cell))) (list ao bo co))
			'new
			'old))

(define-record-type <rlem-3453-state>
	(rlem-3453-state memory a-in b-in c-in a-out b-out c-out)
	rlem-3453-state?
	(memory mem	mem!) ;0,r (default) 1,l
	(a-in ai ai!) (b-in bi bi!) (c-in ci ci!)    ;0 (default) 1
	(a-out ao ao!) (b-out bo bo!) (c-out co co!)
) 

(define tstrlem0 (rlem-3453-state 0 0 0 0 0 0 0))

(define (next-rlem-state rlem)
	(if (and (out-clear? rlem) (in-occupied?))
			(let* ([sym (list (ai rlem) (bi rlem) (ci rlem))]
						 [new-sym (case (mem rlem)
												['0 (list-rotate sym 1)]
												['1 (list-rotate sym 2)])])
				(ao! rlem (a-val new-sym))
				(bo! rlem (b-val new-sym))
				(co! rlem (c-val new-sym))
				(ai! rlem 0) (bi! rlem 0) (ci! rlem 0)
				(mem! rlem (flip-mem (mem rlem)))
				rlem)
			rlem))

;;all outbound channels not blocked
(define (out-clear? rlem)
	(and-map (lambda (p) (zero? (p rlem))) (list ao bo co)))
;;>=1 inbound channels not empty
(define (in-occupied? rlem) 
	(or-map (lambda (p) (not (zero? (p rlem)))) (list ai bi ci)))

(define a-val car) (define b-val cadr) (define c-val caddr)

(define (flip-mem mem) (abs (- mem 1)))

(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))

;;r row by c column large lattice of rlem nodes
(define (rlem-lattice r c)
	(define lattice (cons (map (lambda (a) (rlem-3453-state 0 0 0 0 0 0 0)) 
														 (iota (* r c)))
												`(,universal-perimeter-node)))
	(define universal-perimeter-node (rlem-3453-state 0 0 0 0 0 0 0))		
	(define neighbor-list 'nl #;(hex-grid-neighbors r c))
	(define (sym-inhale rlem-id)
		(let ([nbrs (cdr (assoc rlem-id neighbor-list))])
			
	(define (self msg)
		(case (car msg)
			['lattice lattice]
			['lattice-ref (list-ref lattice (cadr msg))]
			['neighbor-list neighbor-list]
			['neighbor-list! (set! neighbor-list (cadr msg))]
			['mem!-at (mem! (list-ref lattice (cadr msg)) (caddr msg))]
			['sym-in!-at ((case (cadr msg) 
									 ['a ai!] ['b bi!] ['c ci!])
								 (list-ref lattice (caddr msg)) (cadddr msg))]
			['next-state-at (next-rlem-state (list-ref lattice (cadr msg)))]
			['step-lattice-state (for-each 
														(lambda (id) 
															(if (out-clear? (list-ref lattice id))
																	(sym-inhale id))
															(next-rlem-state (list-ref lattice id)))
														(random-permutation (iota (* r c))))]
			[else `(error message ,msg unknown)]
			))
	self)

(define tstlat0 (rlem-lattice 5 1))

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



;;TODO Step through pure functions with lazy evaluation.

;   /c
;a-r0
;   \b

#|
(define (uc-3453-node inp)
	(define state (if (null? inp)
										(uc-3453 _ '(_ _) 'stem 'r '(_ _ _ _ _) 
														 '_ '_ 
														 '_ '_ '_ 
														 '_ '_ '_)
										(uc-3453 (list-ref inp 0)


	(id id id!) ;unique cell identifier
	(position pos pos!) ;co-ordinate pair 

|#
