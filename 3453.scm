;;3453.scm  jpt4  UTC20150905
;;message passing version of RLEM, id 3-453

(define (rlem3453)
	(define role 'stem)
	(define mem 'r)
	(define inp '(_ . _))
	(define nbrs `(_ _ _ ,self))
	(define stin '(_ . _))
	(define buf '())
	(define state `((role . ,role) (mem . ,mem) (inp . ,inp) (nbrs . ,nbrs)))
	(define (become-stem) (self '(set role stem)))		
	(define (circulate)
		(let* ([channel-index (case mem
												 ['r (modulo (+ (car inp) 1) 3)]
												 ['l (modulo (- (car inp) 1) 3)])]
					 [outp (list-ref nbrs channel-index)])
			(outp `(,channel-index . ,(cdr inp)))))
	(define (diagnostic?)	(symbol? (car inp)))
  ;;a native message comprises its input terminal and its numeric payload
	;;0/A 1/B 2/C 3/self
	(define (native?)
		(and (memq (car inp) '(0 1 2 3)) (memq (cdr inp) '(-1 0 1 2 3 4 5))
				 ))		
	(define (become-wire-r)
		(begin
			(self '(set role wire))
			(self '(set mem r))))
	(define (become-wire-l)
		(begin
			(self '(set role wire))
			(self '(set mem l))))
	(define (become-gate-r)
		(begin
			(self '(set role gate))
			(self '(set mem r))))
	(define (become-gate-l)
		(begin
			(self '(set role gate))
			(self '(set mem l))))
	(define (process-native)
		(case role
			['wire (if (eq? (cdr inp) 0)
								 (become-stem)
								 (circulate))]
			['gate (if (eq? (cdr inp) 0)
								 (become-stem)
								 (begin (circulate) (switch-mem)))]
			['stem (case (cdr inp)
							 ['-1 (begin (write-buffer) (parse-buffer))]
							 ['0 (begin (direct-write-buffer-0) (parse-buffer))];parse conditional on buffer length check
							 ;(self `(set buf ,(append buf '(0))))]
							 ['1 (direct-write-buffer-1)]
							 ;(self `(set buf ,(append buf '(1))))]
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
	(define (set)
		(let* ([field (cadr inp)]
					 [value (caddr inp)])
			(begin
				(case field
					['role (set! role value)]
					['mem (set! mem value)]
					['inp (set! inp value)]
					['nbrs (set! nbrs value)])
				(set! state `((role . ,role) (mem . ,mem) (inp . ,inp) (nbrs . ,nbrs)))
				)))				
	(define (switch-mem)
		(case mem
			['r (self '(set mem l))]
			['l (self '(set mem r))]
			[else (dispnl 'bad-switch-mem)]
			))
	(define (self msg)
		(begin
			(set! inp msg)
			(set! state `((role . ,role) (mem . ,mem) (inp . ,inp) (nbrs . ,nbrs)))
			(cond
			 [(native?)	(process-native)]
			 [(diagnostic?) (process-diagnostic)]
			 [else (dispnl 'bad-msg)]
			 )))
	self
	)

(define (dispnl txt)
	(begin
		(display txt)
		(newline)
		))
				
