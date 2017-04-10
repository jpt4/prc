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
(define (all-asm-data uc-asm)
  (list (sta uc-asm) (typ uc-asm) (ups uc-asm) (inp uc-asm)
        (out uc-asm) (mem uc-asm) (aut uc-asm) (con uc-asm) (buf uc-asm)))
(define (wire-data uc-asm)
  (list (sta uc-asm) (ups uc-asm) (inp uc-asm) (out uc-asm) (mem uc-asm)))
(define (proc-data uc-asm)
  (list (ups uc-asm) (inp uc-asm) (out uc-asm) (mem uc-asm)))
(define (stem-data uc-asm)
  (list (typ uc-asm) (ups uc-asm) (inp uc-asm)
        (out uc-asm) (mem uc-asm) (aut uc-asm) (con uc-asm) (buf uc-asm)))

(define (activate asm)
  (let ([asm-data (all-asm-data asm)])
    (match asm-data
           [(qa0 wire ,u ,i ,o ,m ,a ,c ,b)
            (begin
              (sta! asm qw0)
              (wire-step asm))]
           [(qa0 proc ,u ,i ,o ,m ,a ,c ,b)
            (begin
              (sta! asm qp0)
              (proc-step asm))]
           [(qa0 stem ,u ,i ,o ,m ,a ,c ,b)
            (begin
              (sta! asm qs0)
              (stem-step asm))]
           )))

;;monolithic matching
(define (wire-step asm)
  (match (all-asm-data asm)
         [(qw0 ,u ,i ,o ,m) (guard (non-empty? o))
          (halt asm)]
         [(qw0 ,u ,i ,o ,m) (guard (empty? o))
          (begin
            (sta! asm 'qw1)
            (wire-step asm))]
         [(qw1 ,u ,i empty ,m) (guard (empty? u))
          (halt asm)]
         [(qw1 ,u ,i empty ,m) (guard (non-empty? u))
          (begin
            (ups! asm empty) (inp! asm u)
            (wire-step asm))]
         [(qw2 empty ,i empty ,m) (guard (contains-stem-init? i))
          (begin
            (sta! asm 'qw3)
            (typ! asm 'stem) (inp! asm empty) (mem! asm right) 
            (aut! asm empty) (con! asm empty) (buf! asm empty)
            (halt asm))] ;;collapse qw3 due to asm/asm-data mismatch
         [(qw2 empty ,i empty ,m) (guard (all-standard-signals? i))
          (begin
            (sta! asm qw4)
            (wire-step asm))]
         [(qw4 empty ,i empty ,m) (guard (equals? right m))
          (begin
            (sta! asm 'qw5) (inp! asm empty) (out! asm (right-rotate i))
            (wire-step asm))]
         [(qw5 empty empty ,o right)
          (halt asm)]
         [(qw4 empty ,i empty ,m) (guard (equals? left m))
          (begin
            (sta! asm 'qw6) (inp! asm empty) (out! asm (left-rotate i))
            (wire-step asm))]
         [(qw6 empty empty ,o left)
          (halt asm)]           
         ))

;;dispatch to individual functions
(define (proc-step asm)
  (qp0 asm))
(define (qp0 asm)  
  (match (proc-data asm)
         [(,u ,i ,o ,m) (guard (non-empty? o))
          (halt asm)]
         [(,u ,i ,o ,m) (guard (empty? o))
          (qp1 (sta! 'qp1 asm))]))
(define (qp1 asm)
  (match (proc-data asm)
         [(,u ,i empty ,m) (guard (empty? u))
          (halt asm)]
         [(,u ,i empty ,m) (guard (non-empty? u))
          (begin
            (ups! asm empty) (inp! asm u)
            (qp2 (sta! 'qp2 asm)))]))
(define (qp2 asm)
  (match (proc-data asm)
         [(empty ,i empty ,m) (guard (contains-stem-init? i))
          (begin
            (typ! asm 'stem) (inp! asm empty) (mem! asm right) 
            (aut! asm empty) (con! asm empty) (buf! asm empty)
            (qp3 (sta! 'qp3 asm)))] ;no need to collapse qw3 due to (proc-data)
         [(empty ,i empty ,m) (guard (all-standard-signals? i))
          (qp4 (sta! 'qp4 asm))]))
(define (qp3 asm)
  (match (proc-data asm)
         [(stem empty empty empty right empty empty empty)
          (halt asm)]))
(define (qp4 asm)
  (match (proc-data asm)
         [(empty ,i empty ,m) (guard (equals? right m))
          (begin
            (inp! asm empty) (out! asm (right-rotate i))
            (qp5 (sta! 'qp5 asm)))]
         [(empty ,i empty ,m) (guard (equals? left m))
          (begin
            (inp! asm empty) (out! asm (left-rotate i))
            (qp6 (st! 'qp6 asm)))]))
(define (qp5 asm)
  (match (proc-data asm) 
         [(empty empty ,o right)
          (halt asm)]))
(define (qp6 asm)
    (match (proc-data asm)
           [(empty empty ,o left)
            (halt asm)]))

(define (stem-step asm)
  (match (all-asm-data asm)
         [(,s ,t ,u ,i ,o ,m ,a ,c ,b)
          (case s
            [halt "TODO"]
            [qs0 (cond o
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
            [qs9 (case a
                  [wr (typ! wire asm) (inp! empty asm) (mem! right asm)
                      (aut! empty asm) (buf! empty asm) (sta! 'qs10 asm)]
                  [wl (typ! wire asm) (inp! empty asm) (mem! left asm)
                      (aut! empty asm) (buf! empty asm) (sta! 'qs11 asm)]
                  [pr (typ! proc asm) (inp! empty asm) (mem! right asm)
                      (aut! empty asm) (buf! empty asm) (sta! 'qs12 asm)]
                  [pl (typ! proc asm) (inp! empty asm) (mem! left asm)
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
            [qs18 ]
            

(define (qs2 asm)
  (match (stem-data asm)
         ;cannot inline out and buf values in match structure because
         ;they are not syntactic. pattern matching is done as a
         ;convenience to destructure asm data, "state" field is
         ;sufficient to distinguish one from another.
         [(,u ,i ,o ,m ,a (,x ,out) ,b) (guard (non-empty? b))
          (halt asm)]
         [(,u ,i ,o ,m ,a (,x ,out) ,b) (guard (empty? b))
          (begin
            (con! empty asm) 
            (qs3 (sta! 'qs3 asm)))]))
(define (qs3 asm)
  (halt asm))
(define (qs4 asm)
  (match (stem-data asm)
         [(,u ,i empty ,m ,a empty ,b)
          (cond
           [(empty? a) (qs5 (sta! 'qs5 asm))]
           [(non-empty? a) (qs9 (sta! 'qs9 asm))#;process-automail])]))
            
(define (qs5 asm)
  (match (stem-data asm)
         [(,u ,i empty ,m empty empty ,b) (guard (empty? u))
          (halt asm)]
         [(,u ,i empty ,m empty empty ,b) (guard (non-empty? u))
          (begin
            (ups! empty asm) (inp! u asm)
            (qs6 (sta! 'qs6 asm)))]))
(define (qs6 asm)
  (match (stem-data asm)
         [(empty ,i empty ,m empty empty ,b) 
          (guard (or (contains-stem-init? i) (>1-standard-signal? i)))
          (begin
            (inp! empty asm)
            (qs7 (sta! 'qs7 asm)))]
         [(empty ,i empty ,m empty empty ,b) (guard (sss@x? i))
          (begin
            (inp! empty asm) (con! (xin i) asm) (buf! i asm)
            (qs8 (sta! 'qs8 asm)))]))
(define (qs7 asm)
  (match (stem-data asm)
         [(empty empty empty ,m empty emtpy ,b)
          (halt asm)]))
(define (qs8 asm)
  (
          
          
            
