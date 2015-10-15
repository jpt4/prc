;;318.scm  jpt4  UTC20151002
;;RLEM 3-18 is universal with only one port triggering a memory state change.

(define (rlem318)
	(define id 'id)
	(define role 'stem)
	(define mem 'r)
	(define inp '(_ . _))
	(define nbrs `(_ _ _ ,self))
	(define stin '(_ . _))
	(define buf '())
	(define state `((id . ,id) (role . ,role) (mem . ,mem) (inp ,inp) 
									(nbrs . ,nbrs) (stin . ,stin) (buf . ,buf)))
	(define (become-stem) (self '(set role stem)))		
	(define (circulate)
		(let* ([channel-index (case mem
												 ['r (modulo (+ (car inp) 1) 3)]
												 ['l (modulo (- (car inp) 1) 3)])]
					 [outp (list-ref nbrs channel-index)])
			(outp `(,channel-index . ,(cdr inp)))))
	(define (diagnostic?)	(and (not (number? (car inp))) (symbol? (car inp))))
  ;;a native message comprises its input terminal and its numeric payload
	;;0/A 1/B 2/C 3/self
	(define (native?)
		(and (memq (car inp) '(0 1 2 3)) (memq (cdr inp) '(-1 0 1 2 3 4 5))
				 ))		
	(define (become-wire-r)
		(begin (self '(set role wire)) (self '(set mem r))
					 ))
	(define (become-wire-l)
		(begin (self '(set role wire)) (self '(set mem l))
					 ))
	(define (become-gate-r)
		(begin (self '(set role gate)) (self '(set mem r))
					 ))
	(define (become-gate-l)
		(begin (self '(set role gate)) (self '(set mem l))
					 ))
	(define (process-native)
		(case role
			['wire (if (eq? (cdr inp) 0) (become-stem) (circulate))]
			['gate (if (eq? (cdr inp) 0) (become-stem) 
								 (begin (circulate)
												(if (or (and (eq? (car inp) 2) (eq? mem 'l))
																(and (eq? (car inp) 0) (eq? mem 'r)))
														(switch-mem))))]
			['stem (case (cdr inp)
							 ['-1 (begin (write-buffer) (parse-buffer))]
							 ['0 (begin (direct-write-buffer-0) (parse-buffer))]
							 ['1 (direct-write-buffer-1)]
							 ['2 (become-wire-r)]
							 ['3 (become-wire-l)]
							 ['4 (become-gate-r)]
							 ['5 (become-gate-l)]
							 )]
			[else (dispnl 'bad-process-native)]
			))						
	(define (process-diagnostic)
		(case (car inp)
			['show (show)]
			['set (set)]
			))			
	(define (direct-write-buffer-0) (self `(set buf ,(append buf '(0)))))
	(define (direct-write-buffer-1) (self `(set buf ,(append buf '(1)))))
	(define (write-buffer)
		(let ([ent (car inp)]) ;;current input entry terminal
			(begin
				(cond
				 [(eq? (car stin) ent)
					(self `(set buf ,(append buf '(0))))]  ;;inp=-1 on low signal rail
				 [(eq? (cdr stin) ent)
					(self `(set buf ,(append buf '(1))))]  ;;inp=-1 on high signal rail
				 [(and (number? (car stin)) (eq? (cdr stin) '_))  ;;high rail 1st seen
					(begin
						(self `(set stin ,(cons (car stin) ent)))    ;;low rail known
						(self `(set buf ,(append buf '(1))))
						)]
				 [(eq? stin '(_ . _))                             ;;1st seen term
					(self `(set stin ,(cons ent (cdr stin))))       ;;becomes low rail
					]
				 [else (dispnl 'bad-buf-write)]
				 ))))
	(define (parse-buffer)
					 (if (not (eq? (length buf) 5))
							 (begin (dispnl 'bad-parse-buffer) (dispnl buf))
							 (letrec* ([test (list-head buf 2)]
												 [dest (list-ref nbrs (list-binary->decimal test))]
												 [chan (list-binary->decimal (list-head buf 2))]
												 [inst (list-binary->decimal (list-tail buf 2))]
												 )
												(begin
													(self `(set buf ()))
													(dest (cons chan inst))
													))))
	(define (set)
		(let* ([field (cadr inp)]
					 [value (caddr inp)])
			(begin
				(case field
					['role (set! role value)]
					['mem (set! mem value)]
					['inp (set! inp value)]
					['nbrs (set! nbrs value)]
					['stin (set! stin value)]
					['buf (set! buf value)]
					)
				(set! state `((id . ,id) (role . ,role) (mem . ,mem) (inp ,inp) 
									(nbrs . ,nbrs) (stin . ,stin) (buf . ,buf)))
				)))
	(define (show) 
		(case (cadr inp)
			['state state]
			[else (assoc (cadr inp) state)]))
	(define (switch-mem)
		(case mem
			['r (self '(set mem l))]
			['l (self '(set mem r))]
			[else (dispnl 'bad-switch-mem)]
			))
	(define (self msg)
		(begin
			(set! inp msg)
			(set! state `((id . ,id) (role . ,role) (mem . ,mem) (inp ,inp) 
										(nbrs . ,nbrs) (stin . ,stin) (buf . ,buf)))
			(cond
			 [(native?)	(process-native)]
			 [(diagnostic?) (process-diagnostic)]
			 [else (dispnl 'bad-msg)]
			 )))
	self
	)

;;helper functions
(define (dispnl txt)
	(begin (display txt) (newline)
		))
				
(define (list-binary->decimal ls)
	(define (lbd-aux ls len acc)
		(cond
		 [(null? ls) acc]
		 [(eq? (car ls) 0) (lbd-aux (cdr ls) (- len 1) acc)]
		 [(eq? (car ls) 1) (lbd-aux (cdr ls) (- len 1) (+ (expt 2 len) acc))]))
	(lbd-aux ls (- (length ls) 1) 0)
)

