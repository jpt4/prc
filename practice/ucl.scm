;uc.scm
;jpt4
;UTC20151201

;
;uc spec
;
;uclattice records trace of state evolution
;
;ucl
;
;state-at-t := ((time index uc) ...)
;

(define (ucl3453 size)
	(define state-trace
		`((0 . ,(map (lambda (u) (cons u init-uc-state)) (iota size)))))
	(define init-uc-state
		'(sym mem rol buf sig nbr))
	(define current-tick
		(caar state-trace))
	(define (set-total-state dat)
		(set! state-trace (append (list (+ 1 (current-tick)) dat) state-trace)))
	(define (self msg)
		(case (car msg)
			['state-trace state-trace]
			['current-state (car state-trace)]
			['diag-current-tick current-tick]
			['state-at (assoc (cadr msg) state-trace)]
			['set-total-state (set-total-state (cdr msg))]))
	self)

(define ucl0 (ucl3453 1))
(define (tests)
	(display (ucl0 '(state-trace)))
	(display (ucl0 '(current-state)))
	(display (ucl0 '(diag-current-tick)))
	(display (ucl0 '(state-at 0)))
	(ucl0 '(set-total-state (0 _ r (_ _ _ _ _) (_ _) (_ _ _))))
	(display (ucl '(state-trace))))
	
	



		
	
