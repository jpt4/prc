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
	;(rlem3453-direct mem sym)
	(rlem3453-switch mem sym)
	)

(define (rlem3453-direct mem sym)
	(cons (abs (- mem 1)) (list (modulo (+ sym mem 4) 6))))

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
															'()))]
	 ))

(define (rlem33 mem sym)
	(cons mem (list (modulo (+ sym mem 4) 6))))

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
