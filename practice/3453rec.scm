;;3453rec.scm
;;UTC20160115
;;jpt4
;;RLEM-3453 universal cells using record types
;;Guile Scheme v2.2

(use-modules (srfi srfi-9))

(define-record-type <rlem-3453-state>
	(rlem-3453-state memory sym-a sym-b sym-c)
	rlem-3453-state?
	(memory mem	mem!) ;0,r (default) 1,l
	(sym-a sya sya!) (sym-b syb syb!) (sym-c syc syc!)) ;0 (default) 1

(define a-val cadr) (define b-val caddr) (define c-val cadddr)

;;TODO Step through pure functions with lazy evaluation.

;   /c
;a-r0
;   \b
(define r0 (rlem3453 0 1 0 0))
(define r0b (rlem3453 0 0 (b-val r0) 0))
(define r0c (rlem3453 0 0 0 (c-val r0)))



(define (rlem3453 mem a b c)
	(let ([new-sym (case mem
									 ['0 (list-rotate (list a b c) 1)]
									 ['1 (list-rotate (list a b c) 2)])])
		(cons (flip-mem mem) new-sym)))

(define (flip-mem mem) (abs (- mem 1)))

(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))

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
	(role rol rol!) ;stem (default) wire proc
	(buffer buf buf!) ;(_ _ _ _ _) (default)
	(high-rail hig hig!) (low-rail low low!) ;_ (default) a b c 
	(nbr-a nba nba!) (nbr-b nbb nbb!) (nbr-c nbc nbc!) ;_ (default) id
|#
