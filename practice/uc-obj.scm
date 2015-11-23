;uc-obj.scm
;jpt4
;UTC20151123
;Objectified Universal Cells

;simple rlem: (mem x inp) -> (mem x out)
;rlem3453@t=rlem3453(sym rlem3453@t-1)
(define (rlem3453 inp)
	(let* ([sym (car inp)]
				 [mem (cadr inp)]
				 [new-mem (abs (- mem 1))]
				 [new-sym (case mem
										['0 (case sym ;right turn
													['1 5]
													['2 6]
													['3 4])]
										['1 (case sym ;left turn
													['1 6]
													['2 4]
													['3 5])]
										)]
				 )
		(list new-sym new-mem)))

;convert output symbols to input symbols
(define (edge rout)
	(let ([new-insym (case (car rout)
										 ['4 1]
										 ['5 2]
										 ['6 3])
									 ])
		(lambda (m) 
			(list new-insym m))))

(define (node3453 sig mem)
	((edge (rlem3453 sig)) mem))
	
