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
(define amplification-patterns (list '(0 0 1) '(0 1 1) '(1 0 1) '(1 1 1)))

;;universal cell core state
(define-immutable-record-type <uc-core>
  (mk-uc-core inp out smb rol mem ctl buf)
  uc-core?
  (inp cinp^ cinp!) (out cout^ cout!) (smb csmb^ csmb!)
  (rol crol^ crol!) (mem cmem^ cmem!) (ctl cctl^ cctl!) (buf cbuf^ cbuf!))
(define (ai^ core) (list-ref (cinp^ core) 0))
(define (ai! core i) (cinp! core (list-set! (cinp^ core) 0 i)))
(define (bi^ core) (list-ref (cinp^ core) 1))
(define (bi! core i) (cinp! core (list-set! (cinp^ core) 1 i)))
(define (ci^ core) (list-ref (cinp^ core) 2))
(define (ci! core i) (cinp! core (list-set! (cinp^ core) 2 i)))
(define (ao^ core) (list-ref (cout^ core) 0))
(define (ao! core i) (cinp! core (list-set! (cout^ core) 0 i)))
(define (bo^ core) (list-ref (cout^ core) 1))
(define (bo! core i) (cinp! core (list-set! (cout^ core) 1 i)))
(define (co^ core) (list-ref (cout^ core) 2))
(define (co! core i) (cinp! core (list-set! (cout^ core) 2 i)))
;;universal cell node-in-matrix state
(define-immutable-record-type <uc-node>
  (mk-uc-node nid nbra nbrb nbrc inp out smb rol mem ctl buf)
  uc-node?
  (nid nid^ nid!) (nbra nbra^ nbra!) (nbrb nbrb^ nbrb!) (nbrc nbrc^ nbrc!)
  (inp ninp^ ninp!) (out nout^ nout!) (smb nsmb^ nmsb!) (rol nrol^ nrol!)
  (mem nmem^ nmem!) (ctl nctl^ nctl!) (buf nbuf^ nbuf!))
;;universal cell abstract state machine state
;;XX revert to node-core
(define-immutable-record-type <uc-asm>
  (mk-uc-asm ups inp out smb rol mem ctl buf)
  uc-asm?
  (ups aups^ aups!) (inp ainp^ ainp!) (out aout^ aout!) (smb asmb^ asmb!)
  (rol arol^ arol!) (mem amem^ amem!) (ctl actl^ actl!) (buf abuf^ abuf!))
(define (nao^ asm) (list-ref (aups^ asm) 0))
(define (nao! asm v) (aups! asm (list-set! (aups^ asm) 0 v)))
(define (nbo^ asm) (list-ref (aups^ asm) 1))
(define (nbo! asm v) (aups! asm (list-set! (aups^ asm) 1 v)))
(define (nco^ asm) (list-ref (aups^ asm) 2))
(define (nco! asm v) (aups! asm (list-set! (aups^ asm) 2 v)))

;;constructors
(define (make-uc-asm node node-matrix)
  (let* ([core (node-core^ node)]
         [nao (ao^ (node-core^ (matrix-ref node-matrix (nbra^ node))))]
         [nbo (bo^ (node-core^ (matrix-ref node-matrix (nbrb^ node))))]
         [nco (co^ (node-core^ (matrix-ref node-matrix (nbrc^ node))))])
    (mk-uc-asm (list nao nbo nco) (cinp^ core) (cout^ core) (csmb^ core)
               (crol^ core) (cmem^ core) (cctl^ core) (cbuf^ core))))

;;state machine interpretation
(define (step asm)
  (match asm
    ;;wire     
    [($ <uc-asm> ups inp out smb 'wire mem ctl buf)
     (let ([flex-fields (list ups inp out)])
       (match flex-fields
         [(empty empty empty)
          asm]
         [(empty empty (? non-empty? o))
          asm]
         [(empty (? standard-signal? i) empty)
          (set-fields asm [(ainp^) empty] [(aout^) (rotate i (+ 1 mem))])]
         [(empty (? standard-signal? i) (? non-empty? o))
          asm]
         [((? non-empty? u) empty empty)
          (set-fields asm [(aups^) empty] [(ainp^) u])]
         [((? non-empty? u) empty (? non-empty? o))
          (set-fields asm [(aups^) empty] [(ainp^) u])]
         [((? non-empty? u) (? standard-signal? i) empty)
          (set-fields asm [(ainp^) empty] [(aout^) (rotate i (+ 1 mem))])]
         [((? non-empty? u) (? standard-signal? i) (? non-empty? o))
          asm]
         [(u (? special-message? i) empty)
          (process-special-message asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]
    ;;proc
    [($ <uc-asm> ups inp out smb 'proc mem ctl buf)
     (let ([flex-fields (list ups inp out mem)])
       (match flex-fields
         [(empty empty empty m)
          asm]
         [(empty empty (? non-empty? o) m)
          asm]
         [(empty (? standard-signal? i) empty m)
          (set-fields asm 
                      [(ainp^) empty] [(aout^) (rotate i (+ 1 m))] 
                      [(amem^) (switch m)])]
         [(empty (? standard-signal? i) (? non-empty? o) m)
          asm]
         [((? non-empty? u) empty empty m)
          (set-fields asm [(aups^) empty] [(ainp^) u])]
         [((? non-empty? u) empty (? non-empty? o) m)
          (set-fields asm [(aups^) empty] [(ainp^) u])]
         [((? non-empty? u) (? standard-signal? i) empty m)
          (set-fields asm 
                      [(ainp^) empty] [(aout^) (rotate i (+ 1 m))] 
                      [(amem^) (switch m)])]
         [((? non-empty? u) (? standard-signal? i) (? non-empty? o) m)
          asm]
         [(u (? special-message? i) (? empty? o) m)
          (process-special-message asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]
    ;;stem
    [($ <uc-asm> ups inp out smb 'proc mem ctl buf)
     (let ([flex-fields (list ups inp out smb ctl buf)])
       (match flex-fields
         [(empty empty empty clear clear clear)
          asm]
         [((? non-empty? u) empty empty clear c (? <full? b))
          (set-fields asm
                      [(aups^) empty] [(ainp^) u])]
         [(u (? single-standard-signal? i) empty clear clear clear)
          (set-fields asm 
                      [(ainp^) empty] [(actl^) (set-ctl inp)])]
         [(u (? (lambda (a) 
                  (and (single-standard-signal? a)
                       (or (member (list-ref a ctl) standard-signals))))
                i)
             empty clear (? non-clear? c) (? <full? b))
          (set-fields asm
                      [(ainp^) empty] [(abuf^) (write-buf i b)])]
         [(u empty empty clear (? non-clear? c) (? full? b))
          (process-buffer asm)]
         [(u empty (? (lambda (a) (eq? buf a)) o) clear 'one (? amp-pat? b))
          asm]
         [(u empty (? standard-signal? o) clear 'one (? amp-pat? b))
          asm]
         [(u empty empty clear 'one (? amp-pat? b))
          (set-fields asm
                      [(aout^) (set-pat 2 b)] [(actl^) 'two])]
         [(u empty (? standard-signal? o) clear 'two (? amp-pat? b))
          asm]
         [(u empty empty clear 'two (? amp-pat? b))
          (set-fields asm
                      [(aout^) (set-pat 3 b)] [(actl^) 'three])]
         [(u empty (? standard-signal? o) clear 'three (? amp-pat? b))
          asm]
         [(u empty empty clear 'three (? amp-pat? b))
          (set-fields asm
                      [(actl^) clear] [(abuf^) clear])]
         [(u (? special-message? i) empty clear clear clear)
          (process-special-message asm)]
         [(u empty empty (? special-message? s) clear clear)
          (process-self-mailbox asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]         
    [_ 'halt] ;;for diagnostic purposes only
))

(define-values (<full? full?)
  (values
   (lambda (b) (< (length b) max-buffer-length))
   (lambda (b) (eq? (length b) max-buffer-length))))
(define (amp-pat? b)
  (member b amplification-patterns))
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
  (let ([i (car (filter (lambda (a) (member a (list 0 1))) inp))])
    (append buf (list i))))
(define-values (process-buffer bin-dec)
  (values
   (lambda (asm)
     (letrec ([buf (abuf^ asm)] 
              [tar (bin->dec (list-head buf 2))] 
              [cmd (list-ref special-messages (list-tail buf 2))]
              [tmp '(_ _ _)])
       (cond
        [(eq? cmd '(1 1 1)) ;;logical amplifier
         (let ([pat (append tar (list 1))])
           (set-fields asm
                       [(ainp^) (list (car pat) (car pat) (car pat))] 
                       [(actl^) 'one] [(abuf^) pat]))]
        [(zero? tar)
         (set-fields asm
                     [(asmb^) cmd] [(actl^) clear]
                     [(abuf^) clear])]
        [(> tar 0)
         (set-fields asm
                     [(aout^) (list-set! tmp tar cmd)] 
                     [(actl^) clear] [(abuf^) clear])])))
   (lambda (bls)
     (let aux ([b bls] [e (- (length bls) 1)] [acc 0])
       (cond
        [(null? b) acc]
        [else (aux (cdr b) (- e 1) (+ acc (* (car b) (expt 2 e))))])))))
   
(define (process-special-message asm)
  (let* ([rol (arol^ asm)] [inp (ainp^ asm)]
         [sms (car (filter (lambda (a) (member a special-messages)) inp))])
    (case rol
      [(wire) (unless (stem-init? inp)
                      (set-fields asm [(ainp^) empty])
                      (set-stem-defaults asm))]
      [(proc) (unless (stem-init? inp)
                      (set-fields asm [(ainp^) empty])
                      (set-stem-defaults asm))]
      [(stem) (case sms
                [(stem-init) (set-stem-defaults asm)]
                [(wire-r-init) (set-wire-r-defaults asm)]
                [(wire-l-init) (set-wire-l-defaults asm)]
                [(proc-r-init) (set-proc-r-defaults asm)]
                [(proc-l-init) (set-proc-l-defaults asm)]
                )])))
(define (process-self-mailbox asm)
  (let* ([sms (car (asmb^ asm))] [buf (abuf^ asm)])
    (if (member sms (list 0 1))
        (set-fields asm
                    [(asmb^) clear] 
                    [(abuf^) (append buf (list sms))])
        (process-special-message asm))))
;list->num
(define (set-ctl inp) (or (list-index inp 0) (list-index inp 1)))
(define (set-stem-defaults asm)
  (set-fields asm
              [(ainp^) empty] [(aout^) empty] 
              [(asmb^) clear] [(arol^) 'stem]
              [(amem^) 0] [(actl^) clear]
              [(abuf^) clear]))
(define (set-wire-r-defaults asm)
  (set-fields asm
              [(ainp^) empty] [(aout^) empty] 
              [(asmb^) clear] [(arol^) 'wire]
              [(amem^) 0] [(actl^) clear]
              [(abuf^) clear]))
(define (set-wire-l-defaults asm)
  (set-fields asm
              [(ainp^) empty] [(aout^) empty] 
              [(asmb^) clear] [(arol^) 'wire]
              [(amem^) 1] [(actl^) clear]
              [(abuf^) clear]))
(define (set-proc-r-defaults asm)
  (set-fields asm
              [(ainp^) empty] [(aout^) empty] 
              [(asmb^) clear] [(arol^) 'proc]
              [(amem^) 0] [(actl^) clear]
              [(abuf^) clear]))
(define (set-proc-l-defaults asm)
  (set-fields asm
              [(ainp^) empty] [(aout^) empty] 
              [(asmb^) clear] [(arol^) 'proc]
              [(amem^) 1] [(actl^) clear]
              [(abuf^) clear]))
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
  (mk-uc-node 'nid 'nbra 'nbrb 'nbrc 'inp 'out 'smb 'rol 'mem 'ctl 'buf))
(define uc-asm-prototype
  (mk-uc-asm (list 'nao 'nbo 'nco) 'inp 'out 'smb 'rol 'mem 'ctl 'buf))

(define (dispnl t) (begin (display t) (newline)))
(define (dispnl* tls) 
  (cond
   [(and (pair? tls) (pair? (cdr tls))) 
    (begin (dispnl* (car tls)) (for-each dispnl* (cdr tls)))]
   [(and (pair? tls) (null? (cdr tls))) (dispnl* (car tls))]
   [else (dispnl tls)]))
   
;;test suite
(define (tests) 
  (letrec* ([e empty] [c clear] [ne '(_ 0 1)] [ss '(1 0 _)] 
            [si '(stem-init _ 0)]
         [wire-test (mk-uc-asm e e e c 'wire 0 c c)]
         [proc-test (mk-uc-asm e e e c 'proc 0 c c)])
    (begin      
      (dispnl* 
       (list 
        uc-core-prototype  uc-node-prototype uc-asm-prototype
        (set-fields uc-core-prototype
                    [(cctl^) '(0 0 1)]
                    [(cbuf^) '(1 1 1 1)])
        (set-fields uc-node-prototype 
                    [(nbrc^) 3]
                    [(nrol^) 'stem])
        (set-fields uc-asm-prototype 
                    [(amem^) '1])                  
        "wire [ups inp out]"
        "standard signal"
        "e e e"
        (let ([asm wire-test])
          (list asm (step asm)))
        "e e ne"
        (let ([asm (set-fields wire-test [(aout^) ne])])
          (list asm (step asm)))
        "e ss e"
        (let ([asm (set-fields wire-test [(ainp^) ss])])
          (list asm (step asm)))
        "e ss ne"
        (let ([asm (set-fields wire-test 
                               [(ainp^) ss] [(aout^) ne])])
          (list asm (step asm)))
        "ne e e"
        (let ([asm (set-fields wire-test [(aups^) ne])])
          (list asm (step asm)))
        "ne e ne"
        (let ([asm (set-fields wire-test 
                               [(aups^) ne] [(aout^) '(_ 0 1)])])
          (list asm (step asm)))
        "ne ss e"
        (let ([asm (set-fields wire-test 
                               [(aups^) ne] [(ainp^) ss])])
          (list asm (step asm)))
        "ne ss ne"
        (let ([asm (set-fields wire-test 
                               [(aups^) ne] [(ainp^) ss] 
                               [(aout^) ne])])
          (list asm (step asm)))
        "special message"
        "a si e"
        (let ([asm (set-fields wire-test 
                               [(aups^) ne] [(ainp^) si]
                               [(aout^) e])])
          (list asm (step asm)))
        "proc [ups inp out mem]"
        "standard signal"
        "e e e 0"
        (let ([asm proc-test])
          (list asm (step asm)))
        "e e e 1"
        (let ([asm (set-fields proc-test [(amem^) 1])])
          (list asm (step asm)))        
        "e e ne 0"
        (let ([asm (set-fields proc-test [(aout^) ne])])
          (list asm (step asm)))
        "e e ne 1"
        (let ([asm (set-fields proc-test [(aout^) ne] [(amem^) 1])])
          (list asm (step asm)))
        "e ss e 0"
        (let ([asm (set-fields proc-test [(ainp^) ss])])
          (list asm (step asm)))
        "e ss e 1"
        (let ([asm (set-fields proc-test [(ainp^) ss] [(amem^) 1])])
          (list asm (step asm)))        
        "e ss ne 0"
        (let ([asm (set-fields proc-test 
                               [(ainp^) ss] [(aout^) ne])])
          (list asm (step asm)))
        "e ss ne 1"
        (let ([asm (set-fields proc-test 
                               [(ainp^) ss] [(aout^) ne] 
                               [(amem^) 1])])
          (list asm (step asm)))
        "ne e e 0"
        (let ([asm (set-fields proc-test [(aups^) ne] )])
          (list asm (step asm)))
        "ne e e 1"
        (let ([asm (set-fields proc-test [(aups^) ne] [(amem^) 1])])
          (list asm (step asm)))
        "ne e ne 0"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(aout^) '(_ 0 1)])])
          (list asm (step asm)))
        "ne e ne 1"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(aout^) '(_ 0 1)]
                               [(amem^) 1])])
          (list asm (step asm)))
        "ne ss e 0"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(ainp^) ss])])
          (list asm (step asm)))
        "ne ss e 1"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(ainp^) ss]
                               [(amem^) 1])])
          (list asm (step asm)))
        "ne ss ne 0"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(ainp^) ss] 
                               [(aout^) ne])])
          (list asm (step asm)))
        "ne ss ne 1"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(ainp^) ss] 
                               [(aout^) ne] [(amem^) 1])])
          (list asm (step asm)))
        "special message"
        "a si e a"
        (let ([asm (set-fields proc-test 
                               [(aups^) ne] [(ainp^) si]
                               [(aout^) e])])
          (list asm (step asm)))
        )))))
