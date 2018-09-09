;;asmsim.scm
;;jpt4
;;UTC20170307
;;Simulator for Universal Cell abstract state machine model.
;;ChezScheme v9.4.1

;;IU pattern matching
(load "external/match.scm")

;;Universal Cell ASM data structure
(define-record-type uc-asm ;;make-uc-asm uc-asm?
  (fields (mutable state sta sta!)
          (mutable type typ typ!)
          (mutable upstream ups ups!)
          (mutable input inp inp!)
          (mutable output out out!)
          (mutable memory mem mem!)
          (mutable automail aut aut!)
          (mutable control con con!)
          (mutable buffer buf buf!)))

#|
asm internal data structure(s)
('qs0 'stem '(_ _ _) '(_ _ _) '(_ _ _) 'right '_ '() '())
|#
;;default uc-asm construction
(define (make-default-uc-asm)
  (make-uc-asm 'qs0 'stem '(_ _ _) '(_ _ _) '(_ _ _) 'right '_ '() '()))
;;access asm fields
(define (asm-data uc-asm)
  (list (sta uc-asm) (typ uc-asm) (ups uc-asm) (inp uc-asm)
        (out uc-asm) (mem uc-asm) (aut uc-asm) (con uc-asm) (buf uc-asm)))
;;activation
(define (activate asm)
  (match (asm-data asm)
    [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
     (case t
       [wire (sta! asm 'qw0) (wire-step asm)]
       [proc (sta! asm 'qp0) (proc-step asm)]
       [stem (sta! asm 'qs0) (stem-step asm)]
       )]))
;;halt
(define (qhu asm)
  (sta! asm 'halt))
(define (qhuo asm)
  (sta! asm 'halt))
;;wire
(define (wire-step asm)
  (begin
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qw0 (cond 
               [(non-empty? o) (sta! asm 'qhuo)]
               [(empty? o) (sta! asm 'qw1)])]
	 [qw1 (cond
               [(empty? u) (sta! asm 'qhu)]
               [(non-empty? u) (ups! asm empty) (inp! asm u) 
		(sta! asm 'qw2)])]
	 [qw2 (cond
               [(Esi? i) (sta! asm 'qw3) (typ! asm 'stem) (inp! asm empty) 
		(mem! asm right) (aut! asm empty) (con! asm empty) 
		(buf! asm empty)]
               [(ss? i) (sta! asm 'qw4)])]
	 [qw3 (sta! asm 'qhu)]
	 [qw4 (cond
               [(r? m) (inp! asm empty) (out! asm (ir i)) 
		(sta! asm 'qw5)]
               [(l? m) (inp! asm empty) (out! asm (il i)) 
		(sta! asm 'qw6)])]
	 [qw5 (sta! asm 'qhuo)]
	 [qw6 (sta! asm 'qhuo)])])
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qhu (qhu asm)]
	 [qhuo (qhuo asm)]
	 [else (wire-step asm)]
	 )])))
;;proc
(define (proc-step asm)
  (begin
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qp0 (cond 
               [(non-empty? o) (sta! asm 'qhuo)]
               [(empty? o) (sta! asm 'qp1)])]
	 [qp1 (cond
               [(empty? u) (sta! asm 'qhu)]
               [(non-empty? u) (ups! asm empty) (inp! asm u) 
		(sta! asm 'qp2)])]
	 [qp2 (cond
               [(Esi? i) (sta! asm 'qp3) (typ! asm 'stem) (inp! asm empty) 
		(mem! asm right) (aut! asm empty) (con! asm empty) 
		(buf! asm empty)]
               [(ss? i) (sta! asm 'qp4)])]
	 [qp3 (sta! asm 'qhu)]
	 [qp4 (cond
               [(r? m) (inp! asm empty) (out! asm (ir i)) (mem! asm l) 
		(sta! asm 'qp5)]
               [(l? m) (inp! asm empty) (out! asm (il i)) (mem! asm r)
		(sta! asm 'qp6)])]
	 [qp5 (sta! asm 'qhuo)]
	 [qp6 (sta! asm 'qhuo)])])
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qhu (qhu asm)]
	 [qhuo (qhuo asm)]
	 [else (proc-step asm)]
	 )])))
;;stem
(define (stem-step asm)
  (begin
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qs0 (cond 
               [(non-empty? o) (sta! asm 'qhuo)]
               [(empty? o) (sta! asm 'qs1)])]
	 [qs1 (cond
               [(xout? c) (out! asm (b1@x b c)) (buf! asm (b/b1 b))
		(sta! asm 'qs2)]
               [(null? c) (sta! asm 'qs4)]
               [(xin? c) (sta! asm 'qs14)])]
	 [qs2 (cond
               [(non-empty? b) (sta! asm 'qhuo)]
               [(empty? b) (con! empty asm) (sta! asm 'qs3)])]
	 [qs3 (sta! asm 'qhuo)]
	 [qs4 (cond
               [(empty? a) (sta! asm 'qs5)]
               [(non-empty? a) (sta! asm 'qs9)])]
	 [qs5 (cond
               [(empty? u) (sta! asm 'qhu)]
               [(non-empty? u) (ups! asm empty) (inp! asm u) 
		(sta! asm 'qs6)])]
	 [qs6 (cond
               [(Esi-or-ss>1? i) (inp! asm empty) (sta! asm 'qs7)]
               [(sss@x? i) (inp! asm empty) (con! asm (xin i)) 
		(buf! asm (sss-of-i i)) (sta! asm 'qs8)])]
	 [qs7 (sta! asm 'qhu)]
	 [qs8 (sta! asm 'qhu)]
         ;process-automail
	 [qs9 (cond
               [(wr? a) (typ! asm wire) (inp! asm empty) (mem! asm right)
		(aut! asm empty) (buf! asm empty) (sta! asm 'qs10)]
               [(wl? a) (typ! asm wire) (inp! asm empty) (mem! asm left)
		(aut! asm empty) (buf! asm empty) (sta! asm 'qs11)]
               [(pr? a) (typ! asm proc) (inp! asm empty) (mem! asm right)
		(aut! asm empty) (buf! asm empty) (sta! asm 'qs12)]
               [(pl? a) (typ! asm proc) (inp! asm empty) (mem! asm left)
		(aut! asm empty) (buf! asm empty) (sta! asm 'qs13)])]
	 [qs10 (sta! asm 'qhu)]
	 [qs11 (sta! asm 'qhu)]
	 [qs12 (sta! asm 'qhu)]
	 [qs13 (sta! asm 'qhu)]
	 [qs14 (cond
		[(empty? e) (sta! asm 'qhu)]
		[(non-empty? e) (ups! asm empty) (inp! asm u) 
		 (sta! asm 'qs15)])]
	 [qs15 (cond
		[(Esi-or-Esss@!x? i) (inp! asm empty) (sta! asm 'qs16)]
		[(sss@x? i) (inp! asm empty) (buf! asm (b+i b i))
		 (sta! asm 'qs17)])]
	 [qs16 (sta! asm 'qhu)]
	 [qs17 (cond
		[(non-full? b) (sta! asm 'qhu)]
		[(full? b) (sta! asm 'qs18)])]
					;process-buffer
					;need documentation here
	 [qs18 (cond
		[(id+msg? b) (out! asm (b1@xr c b)) (aut! asm (msg b))
		 (con! asm (xrout c)) (buf! asm (b/b1 b)) (sta! asm 'qs19)]
		[(id+10b5? b) (out! asm (b1@xr c b)) (con! asm (xrout c))
		 (buf! asm (b/b1/b5 b)) (sta! asm 'qs20)]
		[(id+11b5? b) (out! asm (b1@xr c b)) (con! asm (xrout c))
		 (buf! asm (b/b1 b)) (sta! asm 'qs21)]
		[(tar+0b4? b) (out! asm (b4@tc c b)) (con! asm (tcout b))
		 (sta! asm 'qs22)]
		[(tar+sic? b) (out! asm (si@tc c)) (con! asm (xnrout c))
		 (sta! asm 'qs23)]
					;Is there space/should no-op control target output?
		[(id+nop? b) (out! asm (b1@xr c b)) (con! asm (xrout c))
		 (buf! asm (b/b1 b)) (sta! asm 'qs24)]
		[(tar+nop? b) (out! asm (b1@tc c b)) (con! asm (tcout b))
		 (buf! asm (b/b1 b)) (sta! asm 'qs25)]
		)]
	 [qs19 (sta! asm 'qhuo)]
	 [qs20 (sta! asm 'qhuo)]
	 [qs21 (sta! asm 'qhuo)]
	 [qs22 (sta! asm 'qhuo)]
	 [qs23 (sta! asm 'qhuo)]
	 [qs24 (sta! asm 'qhuo)]
	 [qs25 (sta! asm 'qhuo)])])
    ;leave or stay within asm hierarchy
    (match (asm-data asm)
      [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
       (case s
	 [qhu (qhu asm)]
	 [qhuo (qhuo asm)]
	 [else (stem-step asm)]
	 )])))
     
;;auxiliary definitions in order of first use
(define empty '(_ _ _))
;empty vs clear vs no-mail?
(define (non-empty? ls) (not (equal? ls empty)))
(define (empty? ls) (equal? ls empty))
(define (Esi? ls) (equal? (filter (lambda (e) (equal? 'si e)) ls) '(si)))
;note - channel valence dependent
(define (ss? i) 
  (equal? 
   (length (filter (lambda (e) (or (equal? 1 e) (equal? 0 e))) ls)) 
   3))                             
(define (r? m) (equal? right m))
(define (ir i) (list (caddr i) (car i) (cadr i)))
(define (l? m) (equal? left m))
(define (ir i) (list (cadr i) (caddr i) (car i)))
;stem auxiliaries
(define (xout? c) (if (not (null? c))
		      (and (number? (car c))
			   (equal? (cadr c) 'out))
		      #f))  
(define (xin? c) (if (not (null? c))
		     (and (number? (car c))
		      (equal? (cadr c) 'in))
		     #f))
(define (b1@x b c) (case (car c)
                     [0 (list (car b) '_ '_)]
                     [1 (list '_ (car b) '_)]
                     [2 (list '_ '_ (car b))]))
(define (b/b1 b) (cdr b))
(define (Esi-or-ss>1? i) 
  (or (member 'si i) 
      (> 1 (length (filter (lambda (a) (or (equal? 1 a) (equal? 0 a))) i)))))
(define (sss@x? i) 
  (and (equal? 1 (length (filter (lambda (a) (or (equal? 1 a) (equal? 0 a))) i)))
       (equal? 2 (length (filter (lambda (a) (equal? '_ a)) i)))))
(define (xin i) (list (max (- 3 (length (member 0 i)))
                           (- 3 (length (member 1 i))))
		      'in))
(define (sss-of-i i) (filter (lambda (a) (or (equal? 1 a) (equal? 0 a))) i))
(define (Esi-or-Esss@!x i c) 'e)
;process-automail auxiliaries
(define (wr? a) (equal? 'wr a))
(define (wl? a) (equal? 'wl a))
(define (pr? a) (equal? 'pr a))
(define (pl? a) (equal? 'pl a))
;process-buffer auxiliaries - XXX CONFIRM MSGLIST CODES
(define (id+msg? b) 
  (let ([id (list-head b 2)]
	[msg (list-tail b 2)])
    (and (equal? id '(0 0))
	 (member msg '(msg-list)))))
(define (id+10b5? b)
  (let ([id (list-head b 2)]
	[10b5 (list-tail b 2)])
    (and (equal? id '(0 0))
	 (or (equal? '(1 0 0) 10b5)
	     (equal? '(1 0 1) 10b5)))))
(define (id+11b5? b)
  (let ([id (list-head b 2)]
	[11b5 (list-tail b 2)])
    (and (equal? id '(0 0))
	 (or (equal? '(1 1 0) 11b5)
	     (equal? '(1 1 1) 11b5)))))
(define (tar+0b4? b)
  (let ([tar (list-head b 2)]
	[0b4 (list-tail b 2)])
    (and (member tar '((0 1) (1 0) (1 1)))
	 (member 0b4 '((0 0) (0 1))))))
(define (tar+sic? b)
  (let ([tar (list-head b 2)]
	[sic (list-tail b 2)])
    (and (member tar '((0 1) (1 0) (1 1)))
	 (equal? sic '(1 1)))))
(define (id+nop? b)
  (or (equal? '(0 0 1 1 0) b) (equal? '(0 0 1 1 1) b)))

;;testing
(define asmtst (make-default-uc-asm))

