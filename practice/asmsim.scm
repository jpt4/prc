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
(define (asm-all-data uc-asm)
  (list (sta uc-asm) (typ uc-asm) (ups uc-asm) (inp uc-asm)
        (out uc-asm) (mem uc-asm) (aut uc-asm) (con uc-asm) (buf uc-asm)))
(define (proc-data uc-asm)
  (list (sta uc-asm) (ups uc-asm) (inp uc-asm) (out uc-asm) (mem uc-asm)))

(define (activate asm)
  (let ([asm-data (asm-all-data asm)])
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
  (let ([asm-data (list
                   (sta asm) (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data
           [(qw0 ,u ,i ,o ,m) (guard (not-empty? o))
            (halt asm)]
           [(qw0 ,u ,i ,o ,m) (guard (empty? o))
            (begin
              (sta! asm 'qw1)
              (wire-step asm))]
           [(qw1 ,u ,i empty ,m) (guard (empty? u))
            (halt asm)]
           [(qw1 ,u ,i empty ,m) (guard (not-empty? u))
            (begin
              (ups! asm empty) (inp! asm u)
              (wire-step asm))]
           [(qw2 empty ,i empty ,m) (guard? (contains-stem-init? i))
            (begin
              (sta! asm 'qw3)
              (typ! asm 'stem) (inp! asm empty) (mem! asm right) 
              (aut! asm empty) (con! asm empty) (buf! asm empty)
              (halt asm))] ;;collapse qw3 due to asm/asm-data mismatch
           [(qw2 empty ,i empty ,m) (guard? (all-standard-signals? i))
            (begin
              (sta! asm qw4)
              (wire-step asm))]
           [(qw4 empty ,i empty ,m) (guard? (equals? right m))
            (begin
              (sta! asm 'qw5) (inp! asm empty) (out! asm (right-rotate i))
              (wire-step asm))]
           [(qw5 empty empty ,o right)
            (halt asm)]
           [(qw4 empty ,i empty ,m) (guard? (equals? left m))
            (begin
              (sta! asm 'qw6) (inp! asm empty) (out! asm (left-rotate i))
              (wire-step asm))]
           [(qw6 empty empty ,o left)
            (halt asm)]           
)))

;;dispatch to individual functions
(define (proc asm)
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (qp0 asm asm-data)))
(define (qp0 asm asm-data)  
    (match asm-data
           [(,u ,i ,o ,m) (guard (not-empty? o))
            (halt asm)]
           [(,u ,i ,o ,m) (guard (empty? o))
            (qp1 asm asm-data)])))
(define (qp1 asm asm-data);;?asm-data a thunk, call per state to get new data?
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data
           [(,u ,i empty ,m) (guard (empty? u))
            (halt asm)]
           [(,u ,i empty ,m) (guard (not-empty? u))
            (begin
              (ups! asm empty) (inp! asm u)
              (qp2 asm))])))
(define (qp2 asm)
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data
           [(empty ,i empty ,m) (guard? (contains-stem-init? i))
            (begin
              (typ! asm 'stem) (inp! asm empty) (mem! asm right) 
              (aut! asm empty) (con! asm empty) (buf! asm empty)
              (qp3 asm))] ;no need to collapse qw3 due to per state asm-data
           [(empty ,i empty ,m) (guard? (all-standard-signals? i))
            (qp4 asm)])))           
(define (qp3 asm)
  (match asm
         [(stem empty empty empty right empty empty empty)
          (halt asm)]))
(define (qp4 asm)
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data
           [(empty ,i empty ,m) (guard? (equals? right m))
            (begin
              (inp! asm empty) (out! asm (right-rotate i))
              (qp5 asm))]
           [(empty ,i empty ,m) (guard? (equals? left m))
            (begin
              (inp! asm empty) (out! asm (left-rotate i))
              (qp6 asm))])))
(define (qp5 asm)
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data  
           [(empty empty ,o right)
            (halt asm)])))
(define (qp6 asm)
  (let ([asm-data (list
                   (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data  
           [(empty empty ,o left)
            (halt asm)])))
                      
           
           


          
