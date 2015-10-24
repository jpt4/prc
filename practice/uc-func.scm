;;universal cell written in functional style
;;jpt4
;;UTC20151024

(define (rlem3453 state symbol)
	(case state
		['r (case symbol
					['a 'a
		['l (rotate -1 symbol)]))

(define (rotate distance sequence)
	(let ([len (length sequence)]
				[piv (mod (
				[dist (abs distance)])
		(if (eq? (* (abs distance) 2) len)
				(list (list-tail sequence (- dist 1)) (list-head sequence (- dist 1)))
				
	 
