;rlem3453.scm
;RLEM 3-453 as function, circuits thereof
;jpt4
;UTC20151204

(define (rlem3453 inp)
	(let* ([mem (car inp)]
				 [sym (cadr inp)]
				 [new-mem (abs (- mem 1))]
				 [new-sym	(case mem
										['0 (rotate sym 1)]
										['1 (rotate sym 2)])]
				 [out (cons new-mem (cons new-sym '()))]
				 )
		out)
)

(define (rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))
		
