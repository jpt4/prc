;;3453.scm  jpt4  UTC20150905
;;object oriented version of RLEM, id 3-453

(define (rlem3453)
	(define role 'stem)
	(define mem 'r)
	(define inp '(_ . _))
	(define nbrs `(_ _ _ ,self))
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
	(define (process-native)
		(case role
			['wire (circulate)]
			))
	(define (set field value) (set! field value))
	(define (self msg)
		(begin
			(set 'inp msg)
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
				
