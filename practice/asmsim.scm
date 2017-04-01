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

(define (activate asm)
  (let ([asm-data (list
                   (sta asm) (typ asm) (ups asm) (inp asm) (out asm)
                   (mem asm) (aut asm) (con asm) (buf asm))])
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

(define (wire-step asm)
  (let ([asm-data (list
                   (sta asm) (ups asm) (inp asm) (out asm) (mem asm))])
    (match asm-data
           [(qw0 ,u ,i ,o ,m) (guard (not-empty? o))
            (halt asm)]
           [(qw0 ,i ,o ,m) (guard (empty? o))
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
           [(qw4 empty ,i empty ,m) 
            (guard? (all-standard-signals? i)) (guard? (equals? right m))
            (begin
              (sta! asm qw5)
              (inp! asm empty) (out! asm (right-rotate i))
                    
                                                      
           
)))
                      
           
           


          
