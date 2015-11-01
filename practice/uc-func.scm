;;universal cell written in functional style
;;jpt4
;;UTC20151024

#|
(define (rlem3453 state)
	(define mem state)
  (define (switch)
		
	
	(define (self msg)
		(case 
			(case state
		['r (case symbol
					['a 'a
		['l (rotate -1 symbol)]))

				self)
(define (rotate distance sequence)
	(let ([len (length sequence)]

				[dist (abs distance)])
		(if (eq? (* (abs distance) 2) len)
				(list (list-tail sequence (- dist 1)) (list-head sequence (- dist 1)))
				)))
|#

;;DEL = Q x SIG -> Q x GAM
(define (rlem3453 mem sym)
	(rlem3453-aux mem sym))

(define (rlem3453-aux mem sym)
	(rlem3453-direct mem sym)
	;(rlem3453-switch mem sym)
	)

(define (rlem3453-direct mem sym)
	(let* ([val (modulo (+ mem sym) 3)]
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
			 

(define (rlem3453-switch mem sym)
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
															'()))]))

(define (rlem3453-buf mem sym rol buf)
	(case rol
		['stm ]
		['wir (cond
					 [(eq? sym 'stem-init) (list mem '_ 'stm '())]
					 [(case mem
						 ['0 (list mem (rotate-sym mem sym) rol buf)]
						 ['1 (list mem (rotate-sym mem sym) rol buf)]
						)])]
								 
		['log (case mem
						['0 (list (mem-invert mem) (rotate-sym mem sym) rol buf)]
						['1 (list (mem-invert mem) (rotate-sym mem sym) rol buf)]
						)]
		))

(define (rlem33 mem sym)
	(let* ([val (modulo (+ mem sym) 3)]
				 [new-sym (modulo (+ (* (sqrt (expt val val)) 4) val) 7)])
		(list mem new-sym)))

(define (exhaust-rlem r)
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
						
(define divisors
	(lambda (n)
		(let f ((i 2))
			(cond
			 [(>= i n) '()]
			 [(integer? (/ n i))
				(begin (display i) (newline)) (f (+ i 1))]
			 [else (f (+ i 1))]))))
