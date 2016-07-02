;;asmsim.scm
;;jpt4
;;An abstract state machine simulation model for Universal Cell matrices.
;;UTC20160502
;;Guile Scheme v.2.0+

;;srfi-9 with immutable extension (define-immutable-record-type)
(use-modules (srfi srfi-9 gnu))
;;pattern matching module (match)
(use-modules (ice-9 match))

;;;universal cell
;;constants
(define standard-signals (list 0 1)) (define max-buffer-length 5) 
(define empty '(_ _ _)) (define clear '())
(define special-messages
  (list 'stem-init 'wire-r-init 'wire-l-init 'proc-r-init 'proc-l-init 0 1))

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
         [((? (lambda (f) (any? 'io f)) u) 
           (? special-message? i) (? empty? o))
          (process-special-message asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]
    ;;proc
    [($ <uc-asm> ups ($ <uc-core> inp out smb 'proc mem ctl buf))
     (let ([flex-fields (list ups inp out mem)])
       (match flex-fields
         [((? empty? u) (? empty? i) (? empty? o) m)
          asm]
         [((? empty? u) (? empty? i) (? non-empty? o) m)
          asm]
         [((? empty? u) (? standard-signal? i) (? empty? o) m)
          (set-fields asm 
                      [(asm-core^ inp^) empty] 
                      [(asm-core^ out^) (rotate i (+ 1 m))] 
                      [(asm-core^ mem^) (switch m)])]
         [((? empty? u) (? standard-signal? i) (? non-empty? o) m)
          asm]
         [((? non-empty? u) (? empty? i) (? empty? o) m)
          (set-fields asm [(ups^) empty] [(asm-core^ inp^) u])]
         [((? non-empty? u) (? empty? i) (? non-empty? o) m)
          (set-fields asm [(ups^) empty] [(asm-core^ inp^) u])]
         [((? non-empty? u) (? standard-signal? i) (? empty? o) m)
          (set-fields asm 
                      [(asm-core^ inp^) empty] 
                      [(asm-core^ out^) (rotate i (+ 1 m))] 
                      [(asm-core^ mem^) (switch m)])]
         [((? non-empty? u) (? standard-signal? i) (? non-empty? o) m)
          asm]
         [((? (lambda (f) (any? 'io f)) u) 
           (? special-message? i) (? empty? o) m)
          (process-special-message asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]
    ;;stem
    [($ <uc-asm> ups ($ <uc-core> inp out smb 'proc mem ctl buf))
     (let ([flex-fields (list ups inp out smb ctl buf)])
       (match flex-fields
         [((? empty? u i o) (? clear? s c b))
          asm]
         [((? non-empty? u) (? empty? i o) (? clear? s) ctl (? <full? b))
          (set-fields asm
                      [(ups^) clear] [(asm-core^ inp^) u])]
         [(u (? single-standard-signal? i) (? empty? o) clear clear clear)
          (set-fields asm 
                      [(asm-core^ inp^) empty] 
                      [(asm-core^ ctl^) (set-ctl inp)])]
         [(u (? (lambda (a) 
                  (and (single-standard-signal? a)
                       (or (eq? (list-ref a (list-ref flex-fields 4)) 0)
                           (eq? (list-ref a (list-ref flex-fields 4)) 1)))) i)
             empty clear (? non-clear? c) (? <full? b))
          (set-fields asm
                      [(asm-core^ inp^) clear] 
                      [(asm-core^ buf^) (write-buf i b)])]
         [(u empty empty clear (? non-clear? c) (? full? b))
          (process-buffer asm)]
         [(u empty (? (lambda (a) (eq? (list-ref flex-fields 5) a)) o) 'one
             clear (? amp-pat? b))
          asm]
         [(u empty (? non-empty? o) 'one clear (? amp-pat? b))
          asm]
         [(u empty empty 'one clear (? amp-pat? b))
          (set-fields asm
                      [(asm-core^ out^) b] [(asm-core^ smb^) 'two])]
         [(u empty (? non-empty? o) 'two clear (? amp-pat? b))
          asm]
         [(u empty empty 'two clear (? amp-pat? b))
          (set-fields asm
                      [(asm-core^ out^) b] [(asm-core^ smb^) 'three])]
         [(u empty empty 'three clear (? amp-pat b))
          (set-fields asm
                      [(asm-core^ smb^) clear] [(asm-core^ buf^) clear])]
         [(u (? special-message? i) empty clear clear clear)
          (process-special-message asm)]
         [(u empty empty (? special-message? s) clear clear)
          (process-self-mailbox asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]         
    [_ 'halt] ;;for diagnostic purposes only
))

(define-values (<full? full)
  (values
   (lambda (b) (< (length b) max-buffer-length))
   (lambda (b) (eq? (length b) max-buffer-length))))
(define (any? typ fel) 
  (case typ
    [(io) 
     (once? 
      true? 
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
(define (non-clear? c) (not (clear? c)))
(define (non-empty? i) (not (empty? i)))
(define (non-full? buf) (< (length buf) max-buffer-length))
(define (standard-signal? i) 
  (and (non-empty? i)
       (and-map (lambda (a) (or (eq? a 1) (eq? a 0) (eq? a '_))) i)))
(define (stem-init? inp) 
  (once? (lambda (a) (and (non-empty? a) (eq? a 'stem-init))) inp))
(define (special-message? i) 
  (count-filter? 1 (lambda (a) (member a special-messages)) i))
(define (true? val) (eq? val #t))
(define (write-buf inp buf) 
  (let ([i (car (filter (lambda (a) (member a (list 0 1))) inp))]
    (append buf (list i))))
(define-values (process-buffer bin-dec)
  (values
   (lambda (asm)
     (letrec ([buf (buf^ (asm-core^ asm))] 
              [tar (bin->dec (list-head buf 2))] 
              [cmd (list-ref special-messages (list-tail buf 2))]
              [tmp '(_ _ _)])
       (cond
        [(eq? cmd '(1 1 1))
         (let ([pat (append tar (list 1))])
           (set-fields asm
                       [(asm-core^ inp^) pat] [(asm-core^ smb^) 1] 
                       [(asm-core^ ctl^) clear] [(asm-core^ buf^) pat]))]
        [(zero? tar)
         (set-fields asm
                     [(asm-core^ smb^) cmd] [(asm-core^ ctl^) clear]
                     [(asm-core^ buf^) clear])]
        [(> tar 0)
         (set-fields asm
                     [(asm-core^ out^) (list-set! tmp tar cmd)] 
                     [(asm-core^ ctl^) clear] [(asm-core^ buf^) clear])])))
   (lambda (bls)
     (let aux ([b bls] [e (- (length bls) 1)] [acc 0])
       (cond
        [(null? b) acc]
        [else (aux (cdr b) (- e 1) (+ acc (* (car b) (expt 2 e))))])))))
   
(define (process-special-message asm)
  (let* ([rol (rol^ (asm-core^ asm))] [inp (inp^ (asm-core^ asm))]
         [sms (car (filter (lambda (a) (member a special-messages)) inp))])
    (case rol
      [(wire) (unless (stem-init? inp)
                      (set-fields asm [(asm-core^ inp^) empty])
                      (set-stem-defaults asm))]
      [(proc) (unless (stem-init? inp)
                      (set-fields asm [(asm-core^ inp^) empty])
                      (set-stem-defaults asm))]
      [(stem) (case sms
                [(stem-init) (set-stem-defaults asm)]
                [(wire-r-init) (set-wire-r-defaults asm)]
                [(wire-l-init) (set-wire-l-defaults asm)]
                [(proc-r-init) (set-proc-r-defaults asm)]
                [(proc-l-init) (set-proc-l-defaults asm)]
                )])))
(define (process-self-mailbox asm)
  (let* ([sms (car (smb^ (asm-core^ asm)))] [buf (buf^ (asm-core^ asm))])
    (if (member sms (list 0 1))
        (set-fields asm
                    [(asm-core^ smb^) clear] 
                    [(asm-core^ buf^) (append buf (list sms))])
        (process-special-message asm))))
(define (set-ctl inp) (or (list-index inp 0) (list-index inp 1)))
(define (set-stem-defaults asm)
  (set-fields asm
              [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
              [(asm-core^ smb^) clear] [(asm-core^ rol^) 'stem]
              [(asm-core^ mem^) 0] [(asm-core^ ctl^) clear]
              [(asm-core^ buf^) clear]))
(define (set-wire-r-defaults asm)
  (set-fields asm
              [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
              [(asm-core^ smb^) clear] [(asm-core^ rol^) 'wire]
              [(asm-core^ mem^) 0] [(asm-core^ ctl^) clear]
              [(asm-core^ buf^) clear]))
(define (set-wire-l-defaults asm)
  (set-fields asm
              [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
              [(asm-core^ smb^) clear] [(asm-core^ rol^) 'wire]
              [(asm-core^ mem^) 1] [(asm-core^ ctl^) clear]
              [(asm-core^ buf^) clear]))
(define (set-proc-r-defaults asm)
  (set-fields asm
              [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
              [(asm-core^ smb^) clear] [(asm-core^ rol^) 'proc]
              [(asm-core^ mem^) 0] [(asm-core^ ctl^) clear]
              [(asm-core^ buf^) clear]))
(define (set-proc-l-defaults asm)
  (set-fields asm
              [(asm-core^ inp^) empty] [(asm-core^ out^) empty] 
              [(asm-core^ smb^) clear] [(asm-core^ rol^) 'proc]
              [(asm-core^ mem^) 1] [(asm-core^ ctl^) clear]
              [(asm-core^ buf^) clear]))
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
         [wire-test (mk-uc-asm e (mk-uc-core e e c 'wire 0 c c))]
         [proc-test (mk-uc-asm e (mk-uc-core e e c 'proc 0 c c))])
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
        "wire [ups inp out]"
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
        "proc [ups inp out mem]"
        "standard signal"
        "e e e 0"
        (let ([asm proc-test])
          (list asm (step asm)))
        "e e e 1"
        (let ([asm (set-fields proc-test [(asm-core^ mem^) 1])])
          (list asm (step asm)))        
        "e e ne 0"
        (let ([asm (set-fields proc-test [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "e e ne 1"
        (let ([asm (set-fields proc-test [(asm-core^ out^) ne] [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "e ss e 0"
        (let ([asm (set-fields proc-test [(asm-core^ inp^) ss])])
          (list asm (step asm)))
        "e ss e 1"
        (let ([asm (set-fields proc-test [(asm-core^ inp^) ss] [(asm-core^ mem^) 1])])
          (list asm (step asm)))        
        "e ss ne 0"
        (let ([asm (set-fields proc-test 
                               [(asm-core^ inp^) ss] [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "e ss ne 1"
        (let ([asm (set-fields proc-test 
                               [(asm-core^ inp^) ss] [(asm-core^ out^) ne] 
                               [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "ne e e 0"
        (let ([asm (set-fields proc-test [(ups^) ne] )])
          (list asm (step asm)))
        "ne e e 1"
        (let ([asm (set-fields proc-test [(ups^) ne] [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "ne e ne 0"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ out^) '(_ 0 1)])])
          (list asm (step asm)))
        "ne e ne 1"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ out^) '(_ 0 1)]
                               [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "ne ss e 0"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ inp^) ss])])
          (list asm (step asm)))
        "ne ss e 1"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ inp^) ss]
                               [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "ne ss ne 0"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ inp^) ss] 
                               [(asm-core^ out^) ne])])
          (list asm (step asm)))
        "ne ss ne 1"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ inp^) ss] 
                               [(asm-core^ out^) ne] [(asm-core^ mem^) 1])])
          (list asm (step asm)))
        "special message"
        "a si e a"
        (let ([asm (set-fields proc-test 
                               [(ups^) ne] [(asm-core^ inp^) si]
                               [(asm-core^ out^) e])])
          (list asm (step asm)))
        )))))
