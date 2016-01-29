;raa.scm
;reconfigurable asynchronous rlem cellular automaton
;jpt4
;UTC20160129
;Guile Scheme v2.2

;;import record types
(use-modules (srfi srfi-9))

;;cell
(define-record-type <rlem-3453-state>
  (rlem-3453-state id role memory a-in b-in c-in a-out b-out c-out buffer
                   high-rail low-rail neighbor-a neighbor-b neighbor-c)
  rlem-3453-state?
  (id id id!) (role rol rol!) (memory mem mem!) 
  (a-in ai ai!) (b-in bi bi!) (c-in ci ci!)
  (a-out ao ao!) (b-out bo bo!) (c-out co co!)
	(buffer buf buf!) (high-rail hig hig!) (low-rail low low!)
  (neighbor-a nba nba!) (neighbor-b nbb nbb!) (neighbor-c nbc nbc!)
)
;;XX OLD UPDATE
(define (update-cell index)
	(let* ([cell (cell-list-ref index)] 
				 [in (lambda () (list (ai cell) (bi cell) (ci cell)))] ;lazy in
				 [out (list (ao cell) (bo cell) (co cell))]           
				 [nbra (cell-list-ref (nba cell))] 
				 [nbrb (cell-list-ref (nbb cell))]
				 [nbrc (cell-list-ref (nbc cell))]
				 [empty? (lambda (c) (equal? '(0 0 0) c))]
				 [inhale ;gather new input from neighbors' outputs
					(lambda () 
						(begin
							(ai! cell (ao nbra)) (bi! cell (bo nbrb)) (ci! cell (co nbrc))
							(ao! nbra 0) (bo! nbrb 0) (co! nbrc 0)))] ;clear nbr outputs
				 [process 
					(lambda ()
						(if (not (empty? (in))) ;did (inhale) acquire new input?
								(begin
									(case (mem cell) ;rotate input right/left, write to output
										['0 (ao! cell (ci cell)) (bo! cell (ai cell)) ;right
												(co! cell (bi cell))]
										['1 (ao! cell (bi cell)) (bo! cell (ci cell)) ;left
												(co! cell (ai cell))])
									(ai! cell 0) (bi! cell 0) (ci! cell 0) ;clear inputs
									(if (equal? (nba cell) 'p) 
											(ao! cell 0))          ;}if cell borders the perimeter
									(if (equal? (nbb cell) 'p) ;}immediately dispose of its
											(bo! cell 0))          ;}output channel values
									(if (equal? (nbc cell) 'p) 
											(co! cell 0))
									(mem! cell (abs (- (mem cell) 1))))))]) ;mem switch
		(cond
		 [(and (empty? out) (empty? (in))) (begin (inhale) (process))]
		 [(and (empty? out) (not (empty? (in)))) (process)]
		 [(and (not (empty? out)) (empty? (in))) (inhale)])
		))
