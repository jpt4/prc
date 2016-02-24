;raa.scm
;reconfigurable asynchronous rlem cellular automaton
;jpt4
;UTC20160129
;Guile Scheme v2.2

;;import record types
(use-modules (srfi srfi-9))

;;cell state
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

(define (cell-list-ref cell-list index) 
	(if (equal? index 'p) 
			(car cell-list)
			(list-ref (cdr cell-list) index)))

;;directions
(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))


#|

Upon activation, a cell checks its current role.

A wire role cell checks if there exists input to process. If none, it
scans its neighbors for potential input, emptying their adjacent
output channels. If still none, the update ends. If input has been
collected, the input is classified. If input contains a mix of special
messages and standard signals, or more than one special message, it is
bad data, all input channels are cleared, and the update ends. If
input is exactly one special message, it is processed, and the update
ends. If input is exactly three standard signals, output channels are
checked. If any output channel is full, the update ends. If all output
channels are empty, the input is processed, and the update ends.

A proc role cell behaves exactly like a wire role cell, except that
when processing standard signal input, it additionally flips its
memory state.

A stem role cell scans for input like a proc or wire cell. If present,
it processes input in A-, B-, C-channel order.

   (Activation)
        |
        |
        #
UPDATE-CELL-BEGIN--role=proc/wire--#SCAN-FOR-INPUT--input=empty--+
        |                                 |                      |
    role=stem                        input=non-empty             #
        |                                 |                UPDATE-CELL-END
        #                                 |                      #    #
  SCAN-FOR-INPUT--input=empty             #                      |    |
        |              |            CLASSIFY-INPUT--input=bad----+    |
   input=non-empty     |                  |      \                    |
        |              #                  |       +--input=special    |
        |        UPDATE-CELL-END          |               |           |
        #                            input=standard       #           |
  PROCESS-STEM-INPUT                      |          PROCESS-SPECIAL--+
      /             \                     #                           |
A-chan=non-empty  A-chan=empty      CHECK-OUTPUT--output=non-empty----+
      |                                   |                           |
      #                             output=empty                      |
PROCESS-A-CHAN                            |                           |
    /     \                               #                           |
                                    PROCESS-STANDARD                  |
                                        /      \                      |
                                       |    role=wire-----------------+
                                   role=proc                          |     
                                       |                              |
                                       #                              |
                                    MEM-FLIP--------------------------+


                    

SCAN-FOR-INPUT:
if {ai, bi, ci} = {0, 0, 0}
then ai <- ao.nbra
     bi <- bo.nbrb
     ci <- co.nbrc
     {ao.nbra, bo.nbrb, co.nbrc} <- {0, 0, 0}
     if {ai, bi, ci} = {0, 0, 0}
     then input <- empty
     else input <- non-empty
else input <- non-empty
   

New Flow Chart

   (Activation)
        |
        |
        #
UPDATE-CELL-BEGIN
        |
        |
        #
   CHECK-INPUT-input=empty-#SCAN-FOR-INPUT-input=empty-#UPDATE-CELL-END
        |                         |                                 #
 input=non-empty           input=non-empty                          |
        |                         |                                 |
        +------------+------------+                                 |
                     |                                              |
                     #                                              |
                CHECK-ROLE-role=wire/proc-#CLASSIFY-INPUT-input=bad-+
                     |                       /         \            |
                 role=stem                  |      input=special    |                                                
                     |                      |           |           |
                     #               input=standard     |           |
             PROCESS-STEM-INPUT             |           #           |
                     |                      |       PROCESS-SPECIAL-+
                     #                      #                       |
              CLASSIFY-INPUT         PROCESS-STANDARD-role=wire-----+
                /         \                 |                       |
               |      input=spcial      role=proc---#MEM-FLIP-------+
               |           |                                        |
        input=standard     |                                        |
               |           #                                        |
               |      STEM-PROCESS-SPECIAL--------------------------+
               #                                                    |
     STEM-PROCESS-STANDARD------------------------------------------+


SCAN-FOR-INPUT:
ai <- ao.nbra
bi <- bo.nbrb
ci <- co.nbrc
{ao.nbra, bo.nbrb, co.nbrc} <- {0, 0, 0}
if {ai, bi, ci} = {0, 0, 0}
   then input <- empty
   else input <- non-empty



|#

(define (update-cell cell-index cell-list)
	(letrec* ([cell (cell-list-ref cell-list cell-index)]
						[in (lambda () (list (ai cell) (bi cell) (ci cell)))] ;lazy in
						[out (list (ao cell) (bo cell) (co cell))]           
						[nbra (cell-list-ref cell-list (nba cell))] 
						[nbrb (cell-list-ref cell-list (nbb cell))] 
						[nbrc (cell-list-ref cell-list (nbc cell))]
						[standard-signal 1]
						[max-buffer-length 5]
						[special-messages 
						 '(stem-init wire-r-init wire-l-init proc-r-init
												 proc-l-init write-buf-zero write-buf-one)]					
						[input-empty? (lambda (c) (equal? '(0 0 0) c))]
						[inhale ;gather new input from neighbors' outputs
						 (lambda () 
							 (begin
								 (ai! cell (ao nbra)) (bi! cell (bo nbrb)) (ci! cell (co nbrc))
								 (ao! nbra 0) (bo! nbrb 0) (co! nbrc 0)))] ;clear nbr outputs
						[update-stem
						 (lambda ()
							 (if (buffer-full?)
									 (buffer-process)
									 (if (input-empty? (in))
											 (begin (inhale) (input-process 'a))
											 (input-process 'a))))]
						[update-wire
						 (lambda ()
							 (begin
								 (case (mem cell) ;rotate input right/left, write to output
									 ['0 (ao! cell (ci cell)) (bo! cell (ai cell)) ;right
											 (co! cell (bi cell))]
									 ['1 (ao! cell (bi cell)) (bo! cell (ci cell)) ;left
											 (co! cell (ai cell))])
								 (ai! cell 0) (bi! cell 0) (ci! cell 0) ;clear inputs
								 (if (equal? (id nba) 'p) 
										 (ao! cell 0))        ;}if any output channels borders the
								 (if (equal? (id nbb) 'p) ;}perimeter immediately dispose of
										 (bo! cell 0))        ;}their outbound values
								 (if (equal? (id nbc) 'p) 
										 (co! cell 0))
								 ))]
						[update-proc (lambda () (begin (update-wire) (mem-switch)))]
						[buffer-full? 
						 (lambda () (equal? (length (buf cell)) max-buffer-length))]
						[buffer-process 
						 (lambda ()
							 (let ([tar (case (list-head (buf cell) 2)
														['(0 0) ao!] ;nbr a
														['(0 1) bo!] ;nbr b
														['(1 0) co!] ;nbr c
														['(1 1) ai!])] ;self
										 [msg (case (list-tail (buf cell) 2)
														['(0 0 0) 'stem-init]
														['(0 0 1) 'wire-r-init]
														['(0 1 0) 'wire-l-init]
														['(0 1 1) 'proc-r-init]
														['(1 0 0) 'proc-l-init]
														['(1 0 1) 'write-buf-zero]
														['(1 1 0) 'write-buf-one]
														['(1 1 1) standard-signal])])
								 (buf! cell '())
								 (tar cell msg)))
						 ]
						[mem-switch (lambda () (mem! cell (abs (- (mem cell) 1))))]
						[clear-channels 
						 (lambda () (begin (ai! cell 0) (bi! cell 0) (ci! cell 0)
															 (ao! cell 0) (bo! cell 0) (co! cell 0)))]
						[standard-signal? (lambda (s) (equal? standard-signal s))]
						[special-message? (lambda (m) (member m special-messages))]
						[input-process
						 (lambda (chan)
							 (case chan
								 ['a (cond
											[(standard-signal? (car (in))) 
											 (begin (process-standard-signal 0) (input-process 'b))]
											[(special-message? (car (in))) 
											 (begin (process-special-message (car (in))) 
															(clear-channels))]
											[else (input-process 'b)])]
								 ['b (cond
											[(standard-signal? (cadr (in))) 
											 (begin (process-standard-signal 1) (input-process 'c))]
											[(special-message? (cadr (in))) 
											 (begin (process-special-message (cadr (in))) 
															(clear-channels))]
											[else (input-process 'c)])]
								 ['c (begin
											 (cond
												[(standard-signal? (caddr (in))) 
												 (process-standard-signal 2)]
												[(special-message? (caddr (in))) 
												 (process-special-message (caddr (in)))])
											 (clear-channels))])								 
							 )]
						[process-standard-signal
						 (lambda (chan)
							 (cond
								[(equal? (hig cell) chan) (write-buf 1)]
								[(equal? (low cell) chan) (write-buf 0)]
								[(equal? (hig cell) '_) (hig! cell chan)]
								[(equal? (low cell) '_) (low! cell chan)]
								))]
						[process-special-message
						 (lambda (msg)
							 (case msg
								 ['stem-init (stem-init)]
								 ['wire-r-init (wire-init 0)]
								 ['wire-l-init (wire-init 1)]
								 ['proc-r-init (proc-init 0)]
								 ['proc-l-init (proc-init 1)]
								 ['write-buf-zero (write-buf 0)]
								 ['write-buf-one (write-buf 1)]))]
						[stem-init (lambda () (begin (rol! cell 'stem) (mem! cell 0) 
																				 (clear-channels)))]
						[wire-init (lambda (m) (begin (rol! cell 'wire) (mem! cell m) 
																					(clear-channels)))]
						[proc-init (lambda (m) (begin (rol! cell 'proc) (mem! cell m)
																					(clear-channels)))]
						[write-buf (lambda (v) 
												 (if (not (buffer-full?))
														 (buf! cell (append (buf cell) (list v)))))]
						)
					 (case (rol cell)
						 ['stem (update-stem)]
						 ['wire (update-wire)]
						 ['proc (update-proc)]
						 )
					 cell))

;;cells do not exist outside of a grid
(define (rlem-hex-grid rows cols)
  (let* ([nbrls (hex-neighbor-list rows cols)]
         [p (rlem-3453-state 'p '_ '_ 0 0 0 0 0 0 '() '_ '_ '_ '_ '_ )]
         [base (cons p (map (lambda (t) (rlem-3453-state 
																				 t 'wire 0 0 0 0 0 0 0 0 0 0 0 0 0))
														(iota (* rows cols))))])
		(map (lambda (e)
					 (nba! (cell-list-ref base (car e)) (cadr e))
					 (nbb! (cell-list-ref base (car e)) (caddr e))
					 (nbc! (cell-list-ref base (car e)) (cadddr e))
					 )
				 nbrls)
		base))

(define (hex-neighbor-list rows cols)
	(let ([grid (unpack (cartesian-product (iota rows) (iota cols)) '())])
		(map
		 (lambda (e)
			 (let* ([r (car e)] [c (cadr e)] [i (+ (* r cols) c)])
				 (cond
					[(even? c) ;point in even column?
					 (list i 
								 (if (zero? c) 'p (west-index i)) ;left column?
								 (if (or (zero? r) (equal? c (- cols 1))) ;bottom row/ 
										 'p                                  ;right column?
										 (south-east-index i cols))
								 (if (equal? c (- cols 1)) ;right-most column?
										 'p 
										 (north-east-index i)))]
					[(odd? c) ;point in odd column?
					 (list i 
								 (if (equal? c (- cols 1)) ;right-most column?
										 'p
										 (east-index i))
								 (if (equal? r (- rows 1)) ;upper row?
										 'p
										 (north-west-index i cols))
								 (south-west-index i))])))
		 grid)))

;f((a ...) (b ...)) = (((a b) (a ...)) (... b) (... ...))
(define (cartesian-product lsa lsb)
	(map (lambda (a) (map (lambda (b) (list a b)) lsb)) lsa))

(define (unpack ls acc)
	(cond
	 [(null? ls) (reverse acc)]
	 [(and (pair? (car ls)) (not (null? (cdar ls))))
		(unpack (cons (cdar ls) (cdr ls)) (cons (caar ls) acc))]
	 [(and (pair? (car ls)) (null? (cdar ls)))
		(unpack (cdr ls) (cons (caar ls) acc))]
	 [else (cons (car ls) (unpack (cdr ls) acc))]))

;;source: http://stackoverflow.com/questions/7313563/flatten-a-list-using-only-the-forms-in-the-little-schemer
;; Similar to SRFI 1's fold
(define (fold1 kons knil lst)
	(if (null? lst)
			knil
			(fold1 kons (kons (car lst) knil) (cdr lst))))

;; Same as R5RS's reverse
(define (reverse lst)
	(fold1 cons '() lst))

;; Helper function
(define (reverse-flatten-into x lst)
	(if (pair? x)
			(fold1 reverse-flatten-into lst x)
			(cons x lst)))

(define (deep-flatten lst)
	  (reverse (reverse-flatten-into lst '())))


#|sophisticated flatten: 
unpack n layers deep, * if unspecified
do/not preserve unpacked '() elements <-- via preprocess tagging?					 
|#

(define (zip lsa lsb)
	(map (lambda (e) (list (list-ref lsa e)
												 (list-ref lsb e)))
			 (iota (length lsa))))

;;XX COLUMN RESET BUG WHEN INCREMENTING ROW
(define (obsolete-hex-neighbor-list rows cols)
	(let ([size (* rows cols)])
		(let next ([i 0] [r 0] [c 0] [nls '()])
      (if (equal? i size) nls ;all points en-neighbored?
					(let ([new-n 
								 (cond
									[(even? c) ;point in even column?
									 (list i 
												 (if (zero? c) 'p (west-index i)) ;left column?
												 (if (or (zero? r) (equal? c (- cols 1))) ;bottom row/ 
														 'p                                  ;right column?
														 (south-east-index i cols))
												 (if (equal? c (- cols 1)) ;right-most column?
														 'p 
														 (north-east-index i)))]
									[(odd? c) ;point in odd column?
									 (list i 
												 (if (equal? c (- cols 1)) ;right-most column?
														 'p
														 (east-index i))
												 (if (equal? r (- rows 1)) ;upper row?
														 'p
														 (north-west-index i cols))
												 (south-west-index i))])])
            (if (<= c cols) ;not end of row?
                (next (+ i 1) r (+ c 1) (append nls (list new-n))) ;next col
                (next (+ i 1) (+ r 1) 0 (append nls (list new-n)))) ;next row
						)))))

