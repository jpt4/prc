;;  3453.scm  jpt4  UTC20150905
;;  object oriented version of RLEM, id 3-453

(define (rlem3453)
	(letrec* ([id 0]
						[role 'stem]  ;;stem gate wire
						[mem 0]
						[buf '()]  ;;stem cell msg buffer
						[terms '(0 0 0 0 0 0)]  ;;A/C' B/A' C/B'
						[nbrs '(0 0 0)]  ;;A/C' B/A' C/B'
						[state `((id . ,id) (role . ,role) (mem . ,mem) (buf . ,buf) 
										 (terms . ,terms) (nbrs . ,nbrs))]
						[state-change
						 (lambda (cur new)
							 (begin
								 (case cur
									 ['id (set! id new)]
									 ['role (set! role new)]
									 ['mem (set! mem new)]
									 ['terms (set! terms new)]
									 ['nbrs (set! nbrs new)]
									 [else 'err])
								 (set! state `((id . ,id) (role . ,role) (mem . ,mem)
															 (buf . ,buf) (terms . ,terms) 
															 (nbrs . ,nbrs)))
								 ))]
						[act (lambda ()
									 'act)]

						#;						[act (lambda ()							
						(case role
						['wire (begin
						(transfer terms nbrs)
						(state-change 'terms '(0 0 0 0 0 0)))]
						['gate (begin
						(transfer terms nbrs)
						(state-change 'terms '(0 0 0 0 0 0))
						(if (eq? mem 0)
						(state-change 'mem 1)
						(state-change 'mem 0)))]
						))]
						[transfer (lambda (sig des)
												(cond
												 [(eq? '(1 0 0 0 0 0) sig)
													(((cadr des) 'set) 'terms '(0 0 0 1 0 0))]
												 [(eq? '(0 0 0 0 0 0) sig) 'nostate-change]))]
						)
					 (lambda (msg)
						 (cond
							[(assoc msg state) (assoc msg state)]
							[(eq? (car msg) 'set)
							 (let ([o (cadr msg)]
										 [n (cddr msg)])
								 (begin
									 (state-change o n)
									 (act)))]
							[else state]))))



(define r1 (rlem3453))
(define r2 (rlem3453))
(r1 '(set nbrs (0 r2 0)))
																				;((r1 'set) 'role 'wire)
