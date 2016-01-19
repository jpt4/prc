;;3453rec.scm
;;UTC20160115
;;jpt4
;;RLEM-3453 universal cells using record types
;;Guile Scheme v2.2

#|
3-phase async co-ordination method:
{A,B,C} = recv-ready, recv-1, send-ready, send-1

rlem<cell<?node

Multi-signal rlem 3453:
IN: MEM A B C OUT: MEM A B C
    0/1  0 0 0     1/0  0 0 0
    0/1  0 0 1     1/0  
    0/1  0 1 0     1/0      
    0/1  0 1 1     1/0      
    0/1  1 0 0     1/0      
    0/1  1 0 1     1/0      
    0/1  1 1 0     1/0      
    0/1  1 1 1     1/0      
|#

(use-modules (srfi srfi-9))

(define-record-type <rlem-3453-state>
	(rlem-3453-state memory sym-a sym-b sym-c)
	rlem-3453-state?
	(memory mem	mem!) ;0,r (default) 1,l
	(sym-a sya sya!) (sym-b syb syb!) (sym-c syc syc!)) ;0 (default) 1

(define-record-type <cell-3453-state>
	(cell-3453-state rlem-state rol buf high-rail low-rail nbr-a nbr-b nbr-c)
	(rlem-state rlem)
	(role rol rol!) ;stem (default) wire proc
	(buffer buf buf!) ;(_ _ _ _ _) (default)
	(high-rail hig hig!) (low-rail low low!) ;_ (default) a b c 
	(nbr-a nba nba!) (nbr-b nbb nbb!) (nbr-c nbc nbc!) ;_ (default) id
)

(define (next-rlem-state rlem)
	(let* ([sym (list (sya rlem) (syb rlem) (syc rlem))]
				[new-sym (case (mem rlem)
									 ['0 (list-rotate sym 1)]
									 ['1 (list-rotate sym 2)])])
		(mem! rlem (flip-mem (mem rlem)))
		(sya! rlem (a-val new-sym))
		(syb! rlem (b-val new-sym))
		(syc! rlem (c-val new-sym))
		rlem))

(define tstrlem0 (rlem-3453-state 0 0 0 0))

(define (rlem-lat s)
	(define lattice (map (lambda (a) (rlem-3453-state '0 '0 '0 '0)) (iota s)))
	(define (self msg)
		(case (car msg)
			['lattice lattice]
			['lattice-ref (list-ref lattice (cadr msg))]
			['mem!-at (mem! (list-ref lattice (cadr msg)) (caddr msg))]
			['sym!-at ((case (cadr msg) 
									 ['a sya!] ['b syb!] ['c syc!])
								 (list-ref lattice (caddr msg)) (cadddr msg))]
			['next-state-at (next-rlem-state (list-ref lattice (cadr msg)))]
			[else `(error message ,msg unknown)]
			))
	self)

(define tstlat0 (rlem-lat 5))

(define a-val car) (define b-val cadr) (define c-val caddr)

(define (flip-mem mem) (abs (- mem 1)))

(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))



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
