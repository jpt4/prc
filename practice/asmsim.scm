;;asmsim.scm
;;jpt4
;;An abstract state machine simulation model for Universal Cell matrices.
;;UTC20160502
;;Guike Scheme v.2.0+

;;srfi-9 with immutable extension (define-immutable-record-type)
(use-modules (srfi srfi-9 gnu))
;;pattern matching module (match)
(use-modules (ice-9 match))

;;;universal cell
;;constants
(define standard-signal 1) (define max-buffer-length 5)
(define empty '(_ _ _)) (define clear '())
(define special-messages 
  (vector 'stem-init 'wire-r-init 'wire-l-init 'proc-r-init 'proc-l-init 
          'zero 'one))

;;universal cell core (aka cell) state
(define-immutable-record-type <uc-core>
  (mk-uc-core inp out smb rol mem ctl buf)
  uc-core?
  (inp inp^ inp!) (out out^ out!) (smb smb^ smb!)
  (rol rol^ rol!) (mem mem^ mem!) (ctl ctl^ ctl!) (buf buf^ buf!))
(define (ai^ core) (list-ref (inp^ core) 0))
(define (ai! core i) (inp! core (list-set! (inp^ core) 0 i)))
(define (bi^ core) (list-ref (inp^ core) 1))
(define (bi! core i) (inp! core (list-set! (inp^ core) 1 i)))
(define (ci^ core) (list-ref (inp^ core) 2))
(define (ci! core i) (inp! core (list-set! (inp^ core) 2 i)))
(define (ao^ core) (list-ref (out^ core) 0))
(define (ao! core i) (inp! core (list-set! (out^ core) 0 i)))
(define (bo^ core) (list-ref (out^ core) 1))
(define (bo! core i) (inp! core (list-set! (out^ core) 1 i)))
(define (co^ core) (list-ref (out^ core) 2))
(define (co! core i) (inp! core (list-set! (out^ core) 2 i)))
;;uc node-in-matrix state
(define-immutable-record-type <uc-node>
  (mk-uc-node nid nbra nbrb nbrc uc-core)
  uc-node?
  (nid nid^ nid!) (nbra nbra^ nbra!) (nbrb nbrb^ nbrb!) (nbrc nbrc^ nbrc!)
  (uc-core node-core^ node-core!))
;;uc abstract state machine state
(define-immutable-record-type <uc-asm>
  (mk-uc-asm ups uc-core)
  uc-asm?
  (ups ups^ ups!) (uc-core asm-core^ asm-core!))
(define (nao^ asm) (list-ref (ups^ asm) 0))
(define (nao! asm v) (ups! asm (list-set! (ups^ asm) 0 v)))
(define (nbo^ asm) (list-ref (ups^ asm) 1))
(define (nbo! asm v) (ups! asm (list-set! (ups^ asm) 1 v)))
(define (nco^ asm) (list-ref (ups^ asm) 2))
(define (nco! asm v) (ups! asm (list-set! (ups^ asm) 2 v)))

;;constructors
(define (make-uc-asm node node-matrix)
  (let* ([core (node-core^ node)]
         [nao (ao^ (node-core^ (matrix-ref node-matrix (nbra^ node))))]
         [nbo (bo^ (node-core^ (matrix-ref node-matrix (nbrb^ node))))]
         [nco (co^ (node-core^ (matrix-ref node-matrix (nbrc^ node))))])
    (mk-uc-asm (list nao nbo nco) core)))

;;state machine interpretation
(define (step asm)
  (match asm
    ;;wire     
    [($ <uc-asm> ups ($ <uc-core> inp out smb 'wire mem ctl buf))
     (let ([flex-fields (list ups inp out)])
       (match flex-fields
         [((? empty? u) (? empty? i) (? empty? o))
          asm]
         [((? empty? u) (? empty? i) (? non-empty? o))
          asm]
         [((? empty? u) (? standard-signal? i) (? empty? o))
          (set-fields asm 
                      [(asm-core^ inp^) empty] [(asm-core^ out^) i])]
         [((? empty? u) (? standard-signal? i) (? non-empty? o))
          asm]
         [((? non-empty? u) (? empty? i) (? empty? o))
          (set-fields asm [(ups^) empty] [(asm-core^ inp^) u])]
         [((? non-empty? u) (? empty? i) (? non-empty? o))
          (set-fields asm [(ups^) empty] [(asm-core^ inp^) u])]
         [((? non-empty? u) (? standard-signal? i) (? empty? o))
          (set-fields asm [(asm-core^ inp^) empty] [(asm-core^ out^) i])]
         [((? non-empty? u) (? standard-signal? i) (? non-empty? o))
          asm]
         [((? (lambda (f) (any? 'io f)) u) (? special-message? i) (? empty? o))
          (process-special-message asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]
    ;;proc
    ;;stem
    [_ 'halt] ;;for diagnostic purposes only
))
#;(define (step asm)
  (match asm
    ;;[ups rol mem ctl buf smb inp out] -> [ups rol mem ctl buf smb inp out]
    ;no-op
    ;e rol mem ctl not-full e e out -> e rol mem ctl not-full e e out
    [($ <uc-asm> 'q0 (? empty? ups) 
        ($ <uc-core> (or 'proc 'wire) mem (? empty? ctl) (? empty? buf) 
           (? empty? smb) (? empty? inp) out))
     asm]
    ;;proc/wire standard signal
    ;;wire
    ;0 1ss 0 -> 0 0 1ss
    [($ <uc-asm> 'q0 (? empty? ups) 
        ($ <uc-core> 'wire mem (? empty? ctl) (? empty? buf) (? empty? smb)
           (? standard inp) (? empty? out)))
     (set-fields asm 
                 [(asm-core inp) (empty inp)]
                 [(asm-core out) (rotate inp (+ 1 mem))])]
    ;;proc
    ;0 1ss 0 -> 0 0 1ss
    [($ <uc-asm> 'q0 (? empty? ups) 
        ($ <uc-core> 'proc mem (? empty? ctl) (? empty? buf) (? empty? smb)
           (? standard inp) (? empty? out)))
     (set-fields asm 
                 [(asm-core inp) (empty inp)]
                 [(asm-core mem) (switch mem)]
                 [(asm-core out) (rotate inp (+ 1 mem))])]
    ;0 1ss 1ne -> 0 1ss 1ne
    [($ <uc-asm> 'q0 (? empty? ups) 
        ($ <uc-core> (or 'proc 'wire) mem (? empty? ctl) (? empty? buf) 
           (? empty? smb)
           (? standard inp) (? not-empty? out)))
     asm]
    ;1ne 0 0 -> 0 1ne 0
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> (or 'proc 'wire) mem (? empty? ctl) (? empty? buf) 
           (? empty? smb)
           (? empty? inp) (? empty? out)))
     (set-fields asm 
                 [(ups) (empty ups)]
                 [(asm-core inp) ups])]
    ;1ne 0 1ne -> 0 1ne 1ne 
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> (or 'proc 'wire) mem (? empty? ctl) (? empty? buf) 
           (? empty? smb)
           (? empty? inp) (? not-empty? out)))
     (set-fields asm 
                 [(ups) (empty ups)]
                 [(asm-core inp) ups])]
    ;;wire - compound transition
    ;1ne 1ss 0 -> 0 1ne 1ss
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> 'wire mem (? empty? ctl) (? empty? buf) (? empty? smb)
           (? standard inp) (? empty? out)))
     (set-fields asm 
                 [(asm-core inp) (empty inp)]
                 [(asm-core out) (rotate inp (+ 1 mem))])]
    ;;proc - compound transition
    ;1ne 1ss 0 -> 0 1ne 1ss
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> 'wire mem (? empty? ctl) (? empty? buf) (? empty? smb)
           (? standard inp) (? empty? out)))
     (set-fields asm 
                 [(asm-core inp) (empty inp)]
                 [(asm-core mem) (switch mem)]
                 [(asm-core out) (rotate inp (+ 1 mem))])]
    ;1ne 1ss 1ne -> 1ne 1ss 1ne
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> 'wire mem (? empty? ctl) (? empty? buf) (? empty? smb)
           (? standard inp) (? not-empty? out)))
     asm]
    ;;proc/wire stem-init signal
    ;ups p/w mem stem? out -> ups 'stem 0 e e
    [($ <uc-asm> 'q0 ups 
        ($ <uc-core> (or 'proc 'wire) mem (? empty? ctl) (? empty? buf) 
           (? empty? smb)
           (? stem-init? inp) out))
     (set-fields asm
                 [(asm-core rol) 'stem] [(asm-core mem) 0] 
                 [(asm-core inp) (empty inp)] [(asm-core out) (empty out)]
                 )]
    ;;stem [ups ctl buf smb inp out]
    ;collect input
    ;ne ctl nf e e out -> e ctl nf e ups out
    [($ <uc-asm> 'q0 (? not-empty? ups) 
        ($ <uc-core> 'stem 0 ctl (? not-full? buf) (? empty? smb)
           (? empty? inp) out))
     (set-fields asm
                 [(ups) (empty ups)] [(asm-core inp) ups])]
    ;establish control rail 
    ;ups e e e sss out -> ups inp e e e out
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 (? empty? ctl) (? empty? buf) (? empty? smb)
           (? single-standard-signal? inp) out))
     (set-fields asm
                 [(inp) (empty inp)] [(asm-core inp) ups])]   
    ;non-final buffer update
    ;ups ne <(full - 1) e sss out -> e ne <=(full - 1) ups out
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 (? not-empty? ctl) (? not-full? buf) 
           (? emtpy? smb) (? single-standard-signal? inp) out))
     (set-fields asm
                 [(ups) (empty ups)] [(asm-core buf) (update-buf ctl inp)]
                 [(asm-core inp) ups])]
    ;final buffer update
    ;ups ne (full - 1) e sss out -> ups ne full e e out
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 (? not-empty? ctl) (? full-less-one? buf) 
           (? emtpy? smb) (? single-standard-signal? inp) out))
     (set-fields asm
                 [(asm-core buf) (update-buf ctl inp)]
                 [(asm-core inp) (empty inp)])]
    ;process full buffer
    ;ups ne full e e e -> ups e e (p buf) e (p buf)
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 (? not-empty? ctl) (? full? buf) (? emtpy? smb)
           (? empty? inp) (? empty? out)))
     (process-buf asm buf)]
    ;respond to self-mail
    ;ups stem 0 e e ne e e -> ups (p smb) (p smb) e e (p smb) e (p smb)
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 (? empty? ctl) (? empty? buf) (? not-emtpy? smb)
           (? empty? inp) (? empty? out)))
     (process-smb asm smb)]
    ;process special message
    ;ups stem 0 ctl buf e sp e -> ups (p inp) (p inp) e e e e (p smb)
    [($ <uc-asm> 'q0 ups
        ($ <uc-core> 'stem 0 ctl buf (? empty? smb)
           (? special? inp) (? empty? out)))
     (process-special-message asm inp)]
    [_ 'halt]
    ))

(define (any? typ fel) 
  (case typ
    [(io) 
     (once? true? 
            (list (empty? fel) (standard-signal? fel) (special-message? fel)))]
    ))
(define (clear? f) (eq? f clear))
(define-values (once? count-filter? count-filter)
  (values
   (lambda (f ls) (count-filter? 1 f ls))
   (lambda (n f ls) (eq? n (count-filter f ls)))
   (lambda (f ls) (length (filter f ls)))))
(define (empty? f) (eq? f empty))
(define (false? val) (eq? val #f))
(define (non-empty? i) (not (empty? i)))
(define (non-full? buf) (< (length buf) max-buffer-length))
(define (standard-signal? i) 
  (and (non-empty? i)
       (and-map (lambda (a) (or (eq? a 1) (eq? a 0) (eq? a '_))) i)))
(define (stem-init? inp) 
  (once? (lambda (a) (and (non-empty? a) (eq? a 'stem-init))) inp))
(define (special-message? i) 
  (count-filter? 1 (lambda (a) (member a (vector->list special-messages))) i))
(define (true? val) (eq? val #t))
(define (process-special-message asm)
  (let* ([rol (rol^ (asm-core^ asm))] [inp (inp^ (asm-core^ asm))])
    (case rol
      [(wire) (unless (stem-init? inp)
                      asm
                      (set-fields asm
                                  [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
                                  [(asm-core^ smb^) clear] [(asm-core^ rol^) 'stem]
                                  [(asm-core^ mem^) 0] [(asm-core^ ctl^) clear]
                                  [(asm-core^ buf^) clear]))]
      [(proc) (dispnl 'proc)]
      [(stem) (dispnl 'stem)])))
(define (switch m) (abs (- m 1)))
(define (rotate l n) (list-rotate l n))
(define (list-rotate ls num)
  (let* ([snum (modulo num (length ls))] ;sanitized shift value
         [new-head (list-tail ls (- (length ls) snum))]
         [new-tail (list-head ls (- (length ls) snum))])
    (append new-head new-tail)))

;;auxiliary
(define-syntax-rule (unless test one two)
  (if test two one))

;;uc matrix
(define (matrix-ref matrix index) (hash-ref matrix index))

;;;test suite
(define uc-core-prototype
  (mk-uc-core 'inp 'out 'smb 'rol 'mem 'ctl 'buf))
(define uc-node-prototype
  (mk-uc-node 'nid 'nbra 'nbrb 'nbrc uc-core-prototype))
(define uc-asm-prototype
  (mk-uc-asm (list 'nao 'nbo 'nco) uc-core-prototype))

(define (dispnl t) (begin (display t) (newline)))
(define (dispnl* tls) 
  (cond
   [(and (pair? tls) (pair? (cdr tls))) 
    (begin (dispnl* (car tls)) (for-each dispnl* (cdr tls)))]
   [(and (pair? tls) (null? (cdr tls))) (dispnl* (car tls))]
   [else (dispnl tls)]))
   
;;test suite
(define (tests) 
  (letrec* ([e empty] [c clear] [ne '(_ 0 1)] [ss '(1 0 _)] [si '(stem-init _ 0)]
         [wire-test (mk-uc-asm e (mk-uc-core e e c 'wire 0 c c))])
    (begin      
      (dispnl* 
       (list 
        uc-core-prototype  uc-node-prototype uc-asm-prototype
        (set-fields uc-core-prototype
                    [(ctl^) '(0 0 1)]
                    [(buf^) '(1 1 1 1)])
        (set-fields uc-node-prototype 
                    [(nbrc^) 3]
                    [(node-core^ rol^) 'stem])
        (set-fields uc-asm-prototype 
                    [(asm-core^ mem^) '1])                  
        "wire"
        "standard signal"
        "e e e"
        (let ([asm wire-test])
          (list asm (step asm)))
        "e e ne"
        (let ([asm (set-fields wire-test [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "e ss e"
        (let ([asm (set-fields wire-test [(asm-core^ inp^) ss])])
          (list asm (step asm)))
        "e ss ne"
        (let ([asm (set-fields wire-test 
                               [(asm-core^ inp^) ss] [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "ne e e"
        (let ([asm (set-fields wire-test [(ups^) ne])])
          (list asm (step asm)))
        "ne e ne"
        (let ([asm (set-fields wire-test 
                               [(ups^) ne] [(asm-core^ out^) '(_ 0 1)])])
          (list asm (step asm)))
        "ne ss e"
        (let ([asm (set-fields wire-test 
                               [(ups^) ne] [(asm-core^ inp^) ss])])
          (list asm (step asm)))
        "ne ss ne"
        (let ([asm (set-fields wire-test 
                               [(ups^) ne] [(asm-core^ inp^) ss] 
                               [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "special message"
        "a si e"
        (let ([asm (set-fields wire-test 
                               [(ups^) ne] [(asm-core^ inp^) si]
                               [(asm-core^ out^) e])])
          (list asm (step asm)))
        )))))
