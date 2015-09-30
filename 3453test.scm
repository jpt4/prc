(define r0 (rlem3453))
(define r1 (rlem3453))
(define r2 (rlem3453)) 
(define r3 (rlem3453)) ;central


(define (test) (begin
(r0 `(set nbrs (,r3 _ _ ,r0)))
(r1 `(set nbrs (_ ,r3 _ ,r1)))
(r2 `(set nbrs (_ _ ,r3 ,r2)))
(r3 `(set nbrs (,r0 ,r1 ,r2 ,r3)))

(r3 '(0 . -1)) ;r0 is low
(r3 '(1 . -1)) ;r1 is high buf = 1
(r3 '(1 . -1)) ;11
(r3 '(0 . -1)) ;110
(r3 '(0 . -1)) ;1100
(r3 '(0 . -1)) ;11000

(r3 '(show state))

(reset r3 '(inp stin buf))
))

(define (reset rob vls)
	(cond
	 [(null? vls) (rob '(show state))]
	 [else (begin
					 (case (car vls)
						 ['id (rob '(set id id))]
						 ['role (rob '(set role ste))]
						 ['mem (rob '(set mem r))]
						 ['stin (rob '(set stin (_ . _)))]
						 ['buf (rob '(set buf ()))])
					 (reset rob (cdr vls)))
				 ]))	 

#|
why?
(eq? (list-head '(1 1 0 0 0) 2) '(1 1))
$25 = #f
eq? tests object, not value equality
use equal? for value
|#

