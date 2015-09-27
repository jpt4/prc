;;3453.scm  jpt4  UTC20150905
;;object oriented version of RLEM, id 3-453

(define (rlem3453)
	(define id 0)
	(define role 'stem)            ;;stem gate wire
	(define mem 0)
	(define buf '())               ;;stem cell msg buffer
	(define stin '(_ . _))         ;;stem cell 0/1 terms
	(define terms '(0 0 0 0 0 0))  ;;A B C A' B' C'
	(define nbrs '(0 0 0))         ;;B/A' C/B' A/C'
	(define state `((id . ,id) (role . ,role) (mem . ,mem) (buf . ,buf) 
										 (stin . ,stin) (terms . ,terms) (nbrs . ,nbrs)))
	(define (state-change cur new)
		(begin
			(case cur
				['id (set! id new)]
				['role (set! role new)]
				['buf (set! buf new)]
				['stin (set! stin new)]
				['mem (set! mem new)]
				['terms (set! terms new)]
				['nbrs (set! nbrs new)]
				[else (dispnl 'err)])
			(set! state `((id . ,id) (role . ,role) (mem . ,mem)
										(buf . ,buf) (stin . ,stin) (terms . ,terms) 
										(nbrs . ,nbrs)))
			))
	(define (act)
		(case role
			['wire (begin (transfer) (transmit) (reset)
										)]
			['gate (begin (transfer) (transmit) (reset) (flip-mem)
										)]
			['stem (if (eq? (length buf) 5)
								 (parse-buf)
								 (let ([inp (list-index terms 1)])
									 (write-buf inp)
									 (dispnl inp)
									 ))]
			[else (dispnl `(,id . no-action))]
			))							
	(define (parse-buf)
		(let* ([dest (case (list-head buf 2)
									 ['(0 0) self]
									 )]
					 [inst (case (list-tail buf 2)
									 ['(0 0 0) `(set terms ,(calc-term-inp 0))]
									 ['(0 0 1) `(set terms ,(calc-term-inp 1))]
									 ['(0 1 0) `ack])])
			'app))
	(define (write-buf inp)
		(cond
		 [(eq? (car stin) inp)
			(state-change 'buf (append buf '(0)))]  ;;inp=0
		 [(eq? (cdr stin) inp)
			(state-change 'buf (append buf '(1)))]  ;;inp=1
		 [(and (number? (car stin)) (eq? (cdr stin) '_))
			(begin
				(state-change 'stin (cons (car stin) inp))
				(state-change 'buf (append buf '(1)))
				)]
		 [(and (number? (cdr stin)) (eq? (car stin) '_))
			(begin
				(state-change 'stin (cons inp (cdr stin)))
				(state-change 'buf (append buf '(0)))
				)]
		 [(eq? stin '(_ . _))
			(begin
				(state-change 'stin (cons inp (cdr stin)))
				(state-change 'buf (append buf '(0)))
				)]
		 [else (dispnl 'bad-buf-write)]
		 ))
	(define (flip-mem)
		(case mem
			['0 (state-change 'mem 1)]
			['1 (state-change 'mem 0)]
			[else (dispnl 'bad-mem-flip)]
			))
	(define (transfer)
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
		 [else (dispnl 'bad-transfer)]
		 ))
	(define (transmit)
		(cond
		 [(> (list-index terms 1) 2)
			(let* ([target (list-ref nbrs (- (list-index terms 1) 3))]
						 [new-terms (append (list (list-ref terms 5)
																			(list-ref terms 3)
																			(list-ref terms 4))
																'(0 0 0))])
				(target `(set terms ,new-terms)))]
		 [else (dispnl 'bad-transmit)]
		 ))
	(define (reset)
		(state-change 'terms '(0 0 0 0 0 0))
		)
	(define (self  msg)
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
		 [(eq? (car msg) 'init)]
		 [else (dispnl 'ill-msg)]))

	self)

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

(define (dispnl txt)  ;;display text followed by newline
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
;	(wire-test)
	)

