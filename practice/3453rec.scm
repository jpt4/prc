;;3453rec.scm
;;UTC20160115
;;jpt4
;;RLEM-3453 universal cells using record types
;;Guile Scheme v2.2

(use-modules (srfi srfi-9))

(define-record-type <uc-3453>
	(uc-3453 
	 id position role memory buffer high-rail low-rail nbr-a nbr-b nbr-c
	 sym-a sym-b sym-c)
	uc-3453?
	(id id id!) ;unique cell identifier
	(position pos pos!) ;co-ordinate pair 
	(role rol rol!) ;stem (default) wire proc
	(memory mem	mem!) ;r (default) l
	(buffer buf buf!) ;(_ _ _ _ _) (default)
	(high-rail hig hig!) (low-rail low low!) ;_ (default) a b c 
	(nbr-a nba nba!) (nbr-b nbb nbb!) (nbr-c nbc nbc!) ;_ (default) id
	(sym-a sya sya!) (sym-b syb syb!) (sym-c syc syc!)) ;_ (default) 

(define (uc-3453-node inp)
	(define state (if (null? inp)
										(uc-3453 _ '(_ _) 'stem 'r '(_ _ _ _ _) 
														 '_ '_ 
														 '_ '_ '_ 
														 '_ '_ '_)
										
