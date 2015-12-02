;uc.scm
;jpt4
;UTC20151201

;
;uc spec
;
;uclattice records trace of state evolution
;
;ucl state grows like lists, from core leftwards.
;state-trace := ((tick-t (index-t (uc-state)) (index-t-1 (uc-state)) ... (index-0 (uc-state)))
;								(tick-t-1 (index-t (uc-state)) (index-t-1 (uc-state)) ... (index-0 (uc-state)))
;								...
;								(tick-0 (index-t (uc-state)) (index-t-1 (uc-state)) ... (index-0 (uc-state)))
;								)
;state-at-t := (tick-t (index-t (uc-state)) (index-t-1 (uc-state)) ... (index-0 (uc-state)))
;
;uc state is fixed size, buffer, signal rail, and neighbor lists written left to right
;uc state := (sym mem rol buf sig nbr)


(define (ucl3453 size)
	(define state-trace
		`((0 . ,(map (lambda (u) (cons u (cons init-uc-state '()))) (iota size)))))
	(define init-uc-state
		'(sym mem rol buf sig nbr))
	(define current-tick
		(caar state-trace))
	(define (add-uc uls) ;uls:=((uc-state)) ...) elements are added to ucl state from head to tail
		(let* ([max-index (cadr (self '(current-state)))]
					 [prefix (map (lambda (u) (cons u ((iota (size uls))
	(define (set-ucl-state dat)
		(set! state-trace (cons (cons (+ 1 current-tick) dat) state-trace)))
;	(define (set-uc-state mod) ;mod:=((index ($FIELD $VALUE) ...) ...)
;		(
	(define (self msg)
		(case (car msg)
			['state-trace state-trace]
			['current-state (car state-trace)]
			['diag-current-tick current-tick]
			['state-at (assoc (cadr msg) state-trace)]
			['set-ucl-state (set-ucl-state (cdr msg))]
			['add-uc (add-uc (cdr msg))]
;			['set-uc-state (set-uc-state (cdr msg))]
			))
	self)

(define ucl0 (ucl3453 1))
(define (tests)
	(display (ucl0 '(state-trace)))	(newline)
	(display (ucl0 '(current-state)))	(newline)
	(display (ucl0 '(diag-current-tick)))	(newline)
	(display (ucl0 '(state-at 0))) (newline)
	(ucl0 '(set-ucl-state (0 (_ r (_ _ _ _ _) (_ _) (_ _ _)))))
	(display (ucl0 '(state-trace)))	(newline)
	(display (ucl0 '(current-state)))	(newline)
	(display (ucl0 '(state-at 1))) (newline)
;	(display (ucl0 '(add-uc (
)
