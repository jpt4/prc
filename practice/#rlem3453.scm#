;rlem3453.scm  jpt4  UTC20151204
;RLEM 3-453, circuits thereof

(define (mk-rlem3453)
	(let* ([mem 0]
				 [sym '(_ _ _)]
				 )
		(define (process-sym sym-inp)
			(let ([new-sym 
						 (case mem
							 ['0 (list-rotate sym-inp 1)]
							 ['1 (list-rotate sym-inp 2)])])
				(flip-mem)
				(set! sym new-sym)
				(self '(get-sym))))
		(define (flip-mem) (set! mem (abs (- mem 1))))
		(define (self msg)
			(case (car msg)
				['sym-recv (process-sym (cadr msg))]
				;gets never change state
				['get-sym sym] 
				['get-mem mem]
				[else "error unknown message"]
				))
		self))

(define (mk-lattice)
	(define lattice '())
	(define (add-cell cell parent-index edge)
		(let* ([parent (if (null? lattice) '() 
											 (cadr (assoc parent-index lattice)))]
					 [new-lattice
						(cond
						 [(null? lattice) `((0 . (,cell _ _ _)))]
						 [(not (null? parent)) 'par]
						 )])
			(set! lattice new-lattice)))
	(define (self msg)
		(case (car msg)
			['add-cell (add-cell (cadr msg) (caddr msg) (cadddr msg))]
			['get-lattice lattice]
			[else "error unknown lattice message"]
			))
	self)
(define rtst (mk-rlem3453))

(define (rlem3453 inp)
	(let* ([mem (car inp)]
				 [sym (cadr inp)]
				 [new-mem (abs (- mem 1))]
				 [new-sym	(case mem
										['0 (list-rotate sym 1)]
										['1 (list-rotate sym 2)])]
				 [out (cons new-mem (cons new-sym '()))]
				 )
		out)
)

(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))
		
(define (a-edge tri)
	(car (cadr tri)))
(define (b-edge tri)
	(cadr (cadr tri)))
(define (c-edge tri)
	(caddr (cadr tri)))

(define (alist-less a b) 
				(let* ([als (cons (cons (car a) a) 
													(cons (cons (car b) b) '()))]) 
					(cdr (assoc (min (car a) (car b)) als))))

;tests
;circuit NEED TO DECIRCULATE
;(define c0 (rlem3453 `(0 (1 ,(b-edge c1) 0))))
;(define c1 (rlem3454 `(0 (0 ,(b-edge c0) 0))))
(sort '((1 . 2) (0 . 5) (3 . 9)) alist-less)
