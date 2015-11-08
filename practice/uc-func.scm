;;universal cell written in functional style
;;jpt4
;;UTC20151024

;;(rlem-X) functions represent the change in UC state individually,
;;and cannot be trivially composed without extra-systemically
;;accounting for interconnection within a UC matrix; thus, they are
;;insufficiently thorough logical specifications. The formal
;;foundation of a PRC machine is a matrix of UCs, each atomically
;;primitive, but also indivisibly a part of a whole.

;;TODO 0 is self, 1,2,3 are A,B,C terminals. Change numbering to
;;reflect.
;;Define different sym-rotate for rlem-buf

;;DEL = Q x SIG -> Q x GAM
(define (rlem3453-base input)
	(rlem3453-base-aux input))

(define (rlem3453-base-aux input)
	(rlem3453-base-direct input)
	;(rlem3453-base-switch input)
	)

(define (rlem3453-base-direct input)
	(let* ([mem (car input)] [sym (cadr input)]
				 [val (modulo (+ mem sym) 3)]
				 [new-sym (modulo (+ (* (sqrt (expt val val)) 4) val) 7)])
		(list (mem-invert mem) new-sym)))

(define (mem-invert m)
	(abs (- m 1)))

(define (sym-rotate m s)
	(case m
	 ['0 (case s ;rotate right
				 ['0 4]
				 ['1 5]
				 ['2 3])]
	 ['1 (case s ;rotate left
				 ['0 5]
				 ['1 3]
				 ['2 4])]))

(define (rlem3453-base-switch input)
	(let* ([mem (car input)] [sym (cadr input)])
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
																'()))])))

(define max-buf 5)
(define null-sym '_)
(define null-buf '())
(define stem-commands (list 'zero 'one 'to-log 'to-wir))

;;STATE = MEMORY x ROLE x BUFFER x HIGH-RAIL x LOW-RAIL
;;DELTA = STATE x SIGMA -> STATE x GAMMA
(define (rlem3453-buf input)
	(let* ([mem (car input)] [sym (cadr input)] [rol (caddr input)]
				 [buf (cadddr input)] [sig '(list-ref input 4)])
		(case rol
			['stm (cond
						 [(or (member? sym sig) (member? (cdr sym) stem-commands))
									(buf-parse mem sym rol (buf-modify sym buf sig) sig)]
						 [(list mem '_ rol buf (sig-update sym sig))]
						 )]
			['wir (cond
						 [(eq? (cadr sym) 'stem-init) (list mem null-sym 'stm '() sig)]
						 [(list mem (sym-rotate mem sym) rol buf sig)]
						 )]
			['log (cond
						 [(eq? sym 'stem-init) (list mem null-sym 'stm '() sig)]
						 [(list (mem-invert mem) (sym-rotate mem sym) rol buf sig)]
						 )]
			)))

;;update buffer according to incoming signal rail
(define (buf-modify sym buf sig)
	(if (< (length buf) (max-buf))
			(cons (list-index sig sym) buf)) ;prepend low/high (0/1) to buffer
	buf)

;;send message to target, according to buffer value
(define (buf-parse mem sym rol buf sig)
	(let* ([tar (list-bin->dec (list-head buf 2))] 
				 [msg (list-tail buf 2)])
		(case msg 
			['(0 0 0) (list mem `(,tar . zero) rol null-buf sig)]
			['(0 0 1) (list mem `(,tar . one) rol null-buf sig)]
			['(0 1 0) (list mem `(,tar . to-log) rol null-buf sig)]			
			['(0 1 1) (list mem `(,tar . to-wir) rol null-buf sig)]			
			['(1 0 0) (list mem `(,tar . stem-init) rol null-buf sig)])))		

;;update which entry terminals are high and low signal rails
;;sig = (low high)
(define (sig-update sym sig)
	(cond
	 [(eq? '_ (car sig)) (list sym (cadr sig))] ;(_ _) or (_ <number>)
	 [(eq? '_ (cdr sig)	(list (car sig) sym)]) ;(<number> _)
	 [else sig])) ;(<number> <number> - defensive, no-op, only reached if 
                ;non-stem-command sym enters on third rail after both signal
                ;rails are allocated.

(define (rlem33-direct input)
	(let* ([mem (car input)] [sym (cadr input)]
				 [val (modulo (+ mem sym) 3)]
				 [new-sym (modulo (+ (* (sqrt (expt val val)) 4) val) 7)])
		(list mem new-sym)))

(define (exhaust-direct-rlem r)
	(let loop ([m 0]
						 [s 0])
		(cond
		 [(and (<= m 1) (<= s 2))
			(begin (display (list `(,m ,s) (r m s)))
						 (newline)
						 (loop (+ m 0) (+ s 1)))]
		 [(and (< m 1) (> s 2))
			(loop (+ m 1) (- s s))])))

(define (universal-3453-circuit) 'u)

(define (cross-product ll)

(define circuit ;;the role emitted by a cell is not the role which affects the
               	;;behavior of its neighbors
	

(define divisors
	(lambda (n)
		(let f ((i 2))
			(cond
			 [(>= i n) '()]
			 [(integer? (/ n i))
				(begin (display i) (newline)) (f (+ i 1))]
			 [else (f (+ i 1))]))))
