;;universal cell written in functional style
;;jpt4
;;UTC20151024

#|
(define (rlem3453 state)
	(define mem state)
  (define (switch)
		
	
	(define (self msg)
		(case 
			(case state
		['r (case symbol
					['a 'a
		['l (rotate -1 symbol)]))

				self)
(define (rotate distance sequence)
	(let ([len (length sequence)]

				[dist (abs distance)])
		(if (eq? (* (abs distance) 2) len)
				(list (list-tail sequence (- dist 1)) (list-head sequence (- dist 1)))
				)))
|#

;;DEL = Q x SIG -> Q x GAM
(define (rlem3453 mem sym)
	(cond
	 [(eq? mem 0) (cons 1 (cons (case sym
																['0 4]
																['1 5]
																['2 3])
															'()))]
	 [(eq? mem 1) (cons 0 (cons (case sym
																['0 5]
																['1 3]
																['2 4])
															'()))]
	 ))

(define (lattice-node) 'ln)
					
