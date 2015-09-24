;;3453.scm  jpt4  UTC20150905
;;object oriented version of RLEM, id 3-453

(define (rlem3453)
	(letrec* ([id 0]
						[role 'stem]            ;;stem gate wire
						[mem 0]
						[buf '()]               ;;stem cell msg buffer
						[stin '(_ . _)]         ;;stem cell 0/1 terms
						[terms '(0 0 0 0 0 0)]  ;;A B C A' B' C'
						[nbrs '(0 0 0)]         ;;B/A' C/B' A/C'
						[state `((id . ,id) (role . ,role) (mem . ,mem) (buf . ,buf) 
										 (stin . ,stin) (terms . ,terms) (nbrs . ,nbrs))]
						[state-change
						 (lambda (cur new)
							 (begin
								 (case cur
									 ['id (set! id new)]
									 ['role (set! role new)]
									 ['buf (set! buf new)]
									 ['stin (set! stin new)]
									 ['mem (set! mem new)]
									 ['terms (set! terms new)]
									 ['nbrs (set! nbrs new)]
									 [else 'err])
								 (set! state `((id . ,id) (role . ,role) (mem . ,mem)
															 (buf . ,buf) (stin . ,stin) (terms . ,terms) 
															 (nbrs . ,nbrs)))
								 ))]
						[act 
						 (lambda ()
							 (case role
								['wire (begin (transfer) (transmit) (reset)
															)]
								['gate (begin (transfer) (transmit) (reset) (flip-mem)
															)]
								['stem (let ([inp (list-ref terms 1)])
												 (cond
													[(eq? (car stin) inp)
													 (state-change 'buf (append buf '(0)))]  ;;inp=0
													[(eq? (cdr stin) inp)
													 (state-change 'buf (append buf '(1)))]  ;;inp=1
													[(and (number? (car stin)) (eq? (cadr stin) '_))
													 (begin
														 (state-change 'stin (cons (car stin) inp))
														 (state-change 'buf (append buf '(1)))
														 )]
													[(and (number? (cadr stin)) (eq? (car stin) '_))
													 (begin
														 (state-change 'stin (cons inp (cdr stin)))
														 (state-change 'buf (append buf '(0)))
														 )]
													[(eq? stin '(_ . _))
													 (begin
														 (state-change 'stin (cons inp (cdr stin)))
														 (state-change 'buf (append buf '(0)))
														 )]
													))]
								[else `(,id . no-action)]
								))]								
						[flip-mem
						 (lambda ()
							 (case mem
								 ['0 (state-change 'mem 1)]
								 ['1 (state-change 'mem 0)]
								 [else 'bad-mem-flip]
								 ))]
						[transfer
						 (lambda ()
							 (cond
								[(eq? '(0 0 0 0 0 0) terms) 'no-transfer]
								[(eq? mem 0)  ;;turn right
								 (state-change
									'terms (append (list-tail terms 3)
																 (list-head terms 3)))]  ;;ABC->A'B'C'
								[(eq? mem 1)  ;;turn left
								 (state-change
									'terms (append '(0 0 0) (list 
																					 (list-ref terms 2)       ;;C->A'
																					 (list-ref terms 0)       ;;A->B'
																					 (list-ref terms 1))))]   ;;B->C'
								[else 'bad-transfer]
								))]
						[transmit 
						 (lambda ()
							 (cond
								[(> (list-index terms 1) 2)
								 (let* ([target (list-ref nbrs (- (list-index terms 1) 3))]
												[new-terms (append (list (list-ref terms 5)
																								 (list-ref terms 3)
																								 (list-ref terms 4))
																					 '(0 0 0))])
									 (target `(set terms ,new-terms)))]
								[else 'bad-transmit]
								))]
						[reset
						 (lambda ()
							 (state-change 'terms '(0 0 0 0 0 0))
							 )]
						)
					 (lambda (msg)
						 (cond
							[(assoc msg state) (assoc msg state)]  ;;report state element
							[(eq? msg 'state) state]							 ;;report all state
							[(eq? msg 'step) (act)]
							[(eq? (car msg) 'set)
							 (let ([o (cadr msg)]
										 [n (caddr msg)])
								 (begin
									 (state-change o n)
									 ))]
							[else 'ill-msg]))))

(define r0 (rlem3453))
(define r1 (rlem3453))
(define r2 (rlem3453))
(define r3 (rlem3453))

(define (disp-all-state)
	(begin
		(display (r0 'state))	(newline)
		(display (r1 'state))	(newline)
		(display (r2 'state))	(newline)
		(display (r3 'state))	(newline)
		))

(define (dispwn txt)
	(begin
		(display txt)
		(newline)))

(define (config)
	(begin
		(display "Tabula rasa") (newline)
		(disp-all-state)
		(r1 '(set id 1))
		(r2 '(set id 2))
		(r3 '(set id 3))
		(r0 `(set nbrs (,r1 ,r2 ,r3)))  ;;B/A'->r1 C/B'->r2 A/C'->r3
		(r1 `(set nbrs (,r0 0 0)))
		(r2 `(set nbrs (0 ,r0 0)))
		(r3 `(set nbrs (0 0 ,r0)))
		(display "Generic config") (newline)
		(disp-all-state)
		))

(define (wire-test)
	(begin
		(r0 '(set role wire))
		(r0 '(set terms (1 0 0 0 0 0)))  ;;term A
		(display "Turn right config") (newline)
		(disp-all-state)
		(r0 'step)		
		(display "Turn right result") (newline)
		(disp-all-state)
		(r0 '(set mem 1))
		(r0 '(set terms (0 1 0 0 0 0)))  ;;term B
		(display "Turn left config") (newline)
		(disp-all-state)
		(r0 'step)
    (display "Turn left result") (newline)
		(disp-all-state)
		))

(begin
	(config)
	(wire-test))

