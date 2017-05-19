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
;;asm internal data structure(s)
#|
('qs0 'stem '(_ _ _) '(_ _ _) '(_ _ _) 'right '_ '() '())
|#
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
  (sta! 'halt asm))
(define (qhuo asm)
  (sta! 'halt asm))
;;wire
(define (wire-step asm)
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
            [qw6 (sta! asm 'qhuo)])
          (case s
            [qhu (qhu asm)]
            [qhuo (qhuo asm)]
            [else (proc-step asm)])
          ]))
;;proc
(define (proc-step asm)
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
            [qp6 (sta! asm 'qhuo)])
          (case s
            [qhu (qhu asm)]
            [qhuo (qhuo asm)]
            [else (proc-step asm)])
          ]))
;;stem
(define (stem-step asm)
  (match (asm-data asm)
         [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
          (case s
            [qs0 (cond 
                  [(not-empty? o) (sta! 'qhuo asm)]
                  [(empty? o) (sta! 'qs1 asm)])]
            [qs1 (cond
                  [(xout? c) (out! (b1@x b c) asm) (buf! (b/b1 b) asm)
                   (sta! 'qs2 asm)]
                  [(empty? c) (sta! 'qs4 asm)]
                  [(xin? c) (sta! 'qs14 asm)])]
            [qs2 (cond
                  [(non-empty? b) (sta! 'qhuo asm)]
                  [(empty? b) (con! empty asm) (sta! 'qs3 asm)])]
            [qs3 (sta! 'qhuo asm)]
            [qs4 (cond
                  [(empty? a) (sta! 'qs5 asm)]
                  [(non-empty? a) (sta! 'qs9 asm)])]
            [qs5 (cond
                  [(empty? u) (sta! 'qhu asm)]
                  [(non-empty? u) (ups! empty asm) (inp! u asm) 
                   (sta! 'qs6 asm)])]
            [qs6 (cond
                  [(Esi-or-ss>1? i) (inp! empty asm) (sta! 'qs7 asm)]
                  [(sss@x? i) (inp! empty asm) (con! (xin i) asm) 
                   (buf! (sss-of-i i) asm) (sta! 'qs8 asm)])]
            [qs7 (sta! 'qhu asm)]
            [qs8 (sta! 'qhu asm)]
            ;process-automail
            [qs9 (cond
                  [(wr? a) (typ! wire asm) (inp! empty asm) (mem! right asm)
                   (aut! empty asm) (buf! empty asm) (sta! 'qs10 asm)]
                  [(wl? a) (typ! wire asm) (inp! empty asm) (mem! left asm)
                   (aut! empty asm) (buf! empty asm) (sta! 'qs11 asm)]
                  [(pr? a) (typ! proc asm) (inp! empty asm) (mem! right asm)
                   (aut! empty asm) (buf! empty asm) (sta! 'qs12 asm)]
                  [(pl? a) (typ! proc asm) (inp! empty asm) (mem! left asm)
                   (aut! empty asm) (buf! empty asm) (sta! 'qs13 asm)])]
            [qs10 (sta! 'qhu asm)]
            [qs11 (sta! 'qhu asm)]
            [qs12 (sta! 'qhu asm)]
            [qs13 (sta! 'qhu asm)]
            [qs14 (cond
                   [(empty? e) (sta! 'qhu asm)]
                   [(non-empty? e) (ups! empty asm) (inp! u asm) 
                    (sta! 'qs15 asm)])]
            [qs15 (cond
                   [(Esi-or-Esss@!x? i) (inp! empty asm) (sta! 'qs16 asm)]
                   [(sss@x? i) (inp! empty asm) (buf! (b+i b i) asm)
                    (sta! 'qs17 asm)])]
            [qs16 (sta! 'qhu asm)]
            [qs17 (cond
                   [(non-full? b) (sta! 'qhu asm)]
                   [(full? b) (sta! 'qs18 asm)])]
            ;process-buffer
            ;need documentation here
            [qs18 (cond
                   [(id+msg? b) (out! (b1@xr c b) asm) (aut! (msg b) asm)
                    (con! (xrout c) asm) (buf! (b/b1 b) asm) (sta! 'qs19 asm)]
                   [(id+10b5? b) (out! (b1@xr c b) asm) (con! (xrout c) asm)
                    (buf! (b/b1/b5 b) asm) (sta! 'qs20 asm)]
                   [(id+11b5? b) (out! (b1@xr c b) asm) (con! (xrout c) asm)
                    (buf! (b/b1 b) asm) (sta! 'qs21 asm)]
                   [(tar+0b4? b) (out! (b4@tc c b) asm) (con! (tcout b) asm)
                    (sta! 'qs22 asm)]
                   [(tar+sic? b) (out! (si@tc c) asm) (con! (xnrout c) asm)
                    (sta! 'qs23 asm)]
                   ;Is there space/should no-op control target output?
                   [(id+nop? b) (out! (b1@xr c b) asm) (con! (xrout c) asm)
                    (buf! (b/b1 b) asm) (sta! 'qs24 asm)]
                   [(tar+nop? b) (out! (b1@tc c b) asm) (con! (tcout b) asm)
                    (buf! (b/b1 b) asm) (sta! 'qs25 asm)]
                   )]
            [qs19 (sta! 'qhuo asm)]
            [qs20 (sta! 'qhuo asm)]
            [qs21 (sta! 'qhuo asm)]
            [qs22 (sta! 'qhuo asm)]
            [qs23 (sta! 'qhuo asm)]
            [qs24 (sta! 'qhuo asm)]
            [qs25 (sta! 'qhuo asm)])
          (case s ;leave or stay within asm hierarchy
            [qhu (qhu asm)]
            [qhuo (qhuo asm)]
            [else (stem-step asm)])
          ]))
;;auxiliary definitions in order of first use
(define empty '(_ _ _)
;empty vs clear vs no-mail?
(define (non-empty? ls) (not (eq? ls empty)))
(define (empty? ls) (eq? ls empty))
(define (Esi? ls) (eq? (filter (lambda (e) (eq? 'si e)) ls) '(si)))
;note - channel valence dependent
(define (ss? i) (eq? (length (filter (lambda (e) (or (eq? 1 e) (eq? 0 e))) ls))
                     3))                             
(define (r? m) (eq? right m))
(define (ir i) (list (caddr i) (car i) (cadr i)))
(define (l? m) (eq? left m))
(define (ir i) (list (cadr i) (caddr i) (car i)))
;stem auxiliaries
(define (xout? c) (and (number? (car c))
                       (eq? (cadr c) 'out)))
(define (xin? c) (and (number? (car c))
                       (eq? (cadr c) 'in)))
(define (b1@x b c) (case (car c)
                     [0 (list (car b) '_ '_)]
                     [1 (list '_ (car b) '_)]
                     [2 (list '_ '_ (car b))]))
(define (b/b1 b) (cdr b))
(define (Esi-or-ss>1? i) 
  (or (member 'si i) 
      (> 1 (length (filter (lambda (a) (or (eq? 1 a) (eq? 0 a))) i)))))
(define (sss@x? i) 
  (and (eq? 1 (length (filter (lambda (a) (or (eq? 1 a) (eq? 0 a))) i)))
       (eq? 2 (length (filter (lambda (a) (eq? '_ a)) i)))))
(define (xin i) (list (max (- 3 (length (member 0 i)))
                           (- 3 (length (member 1 i))))
                      'in))
(define (sss-of-i i) (filter (lambda (a) (or (eq? 1 a) (eq? 0 a))) i))
(define (Esi-or-Esss@!x i c) 'e)
;process-automail auxiliaries
(define (wr? a) (eq? 'wr a)
(define (wl? a) (eq? 'wl a)
(define (pr? a) (eq? 'pr a)
(define (pl? a) (eq? 'pl a)
;process-buffer auxiliaries - XXX CONFIRM MSGLIST CODES
(define (id+msg? b) 
  (let ([id (list-head b 2)]
        [msg (list-tail b 2)])
    (and (eq? id '(0 0))
         (member msg '(msg-list)))))
(define (id+10b5? b)
  (let ([id (list-head b 2)]
        [10b5 (list-tail b 2)])
    (and (eq? id '(0 0))
         (or (eq? '(1 0 0) 10b5)
             (eq? '(1 0 1) 10b5)))))
(define (id+11b5? b)
  (let ([id (list-head b 2)]
        [11b5 (list-tail b 2)])
    (and (eq? id '(0 0))
         (or (eq? '(1 1 0) 11b5)
             (eq? '(1 1 1) 11b5)))))
(define (tar+0b4? b)
  (let ([tar (list-head b 2)]
        [0b4 (list-tail b 2)])
    (and (member tar '((0 1) (1 0) (1 1)))
         (member 0b4 '((0 0) (0 1))))))
(define (tar+sic? b)
  (let ([tar (list-head b 2)]
        [sic (list-tail b 2)])
    (and (member tar '((0 1) (1 0) (1 1)))
         (eq? sic '(1 1)))))
(define (id+nop? b)
  (eq? '(0 0 

        
                 
