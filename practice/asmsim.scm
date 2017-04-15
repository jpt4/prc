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
(define (asm-data uc-asm)
  (list (sta uc-asm) (typ uc-asm) (ups uc-asm) (inp uc-asm)
        (out uc-asm) (mem uc-asm) (aut uc-asm) (con uc-asm) (buf uc-asm)))

(define (activate asm)
  (match (asm-data asm)
         [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
          (case t
            [wire (sta! asm 'qw0) (wire-step asm)]
            [proc (sta! asm 'qp0) (proc-step asm)]
            [stem (sta! asm 'qs0) (stem-step asm)]
            ))))
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
                  [(empty? c) (sta! 'qs4 asm)])]
            [qs2 (cond
                  [(non-empty? b) (sta! 'qhuo asm)]
                  [(empty? b) (con! empty asm) (sta! 'qs3 asm)])]
            [qs3 (sta! 'qhuo asm)]
            [qs4 (cond
                  [(empty? a) (sta! 'qs5 asm)]
                  [(non-empty? a) (sta! 'qs9 asm)]
                  [(xin? c) (sta! 'qs14 asm)])]
            [qs5 (cond
                  [(empty? u) (sta! 'qhu asm)]
                  [(non-empty? u) (ups! empty asm) (inp! u asm) 
                   (sta! 'qs6 asm)])]
            [qs6 (cond
                  [(Esi-or-ss>1? i) (inp! empty asm) (sta! 'qs7 asm)]
                  [(sss@x? i) (inp! empty asm) (con! (xin i) asm) 
                   (buf! (extract-signal i) asm) (sta! 'qs8 asm)])]
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
            [(stem-step asm)])
          ]))
;;auxiliary functions
(define (non-empty? ls) (not (null? ls)))
(define (empty? ls) (null? ls))
(define (Esi? ls) (count 'si ls))
(define (count sym ls) 
  (length (filter (lambda (e) (eq? sym e)) ls)))
