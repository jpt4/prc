;;  390.scm  jpt4  UTC20150905
;;  object oriented version of RLEM, id 3-90

(define (rlem390)
	(let* ([id 0]
				 [role 'stem]
				 [mem 0]
				 [terms '(0 0 0 0 0 0)]
				 [nbrs '(0 0 0 0 0 0)]
				 [state `((id . ,id) (role . ,role) (mem . ,mem) (terms . ,terms) 
									(nbrs . ,nbrs))]
				 [change (lambda (cur new)
									 (begin
										 (case cur
											 ['id (set! id new)]
											 ['role (set! role new)]
											 ['terms (set! terms new)]
											 ['nbrs (set! nbrs new)]
											 [else 'err])
										 (set! state `((id . ,id) (role . ,role) (mem . ,mem)
																	 (terms . ,terms) (nbrs . ,nbrs)))
										 (act)))]
				 [calculate-state (lambda () 0)]
				 [act (lambda ()
								(let ([stat (calculate-state)])
									(case stat
										[else stat]
										)))]
				 )
		(lambda (msg)
			(cond
			 [(assoc msg state) (assoc msg state)]
			 [(eq? msg 'set) (lambda (old mod) (change old mod))]
			 [else state]))))

(define r1 (rlem390))
