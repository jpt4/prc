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
(define clear '()) (define empty '(_ _ _)) (define self-target-index 3)
(define standard-signals (list 0 1)) (define max-buffer-length 5) 
(define special-messages
  (list 
   'stem-init 'wire-r-init 'wire-l-init 'proc-r-init 'proc-l-init))
(define amplification-patterns (list '(0 0 1) '(0 1 1) '(1 0 1) '(1 1 1)))

;;universal cell core state
(define-immutable-record-type <uc-core>
  (mk-uc-core inp out smb rol mem ctl buf)
  uc-core?
  (inp cinp^) (out cout^) (smb csmb^) (rol crol^) 
  (mem cmem^) (ctl cctl^) (buf cbuf^))
(define (ai^ core) (list-ref (cinp^ core) 0))
;(define (ai! core i) (cinp! core (list-set! (cinp^ core) 0 i)))
(define (bi^ core) (list-ref (cinp^ core) 1))
;(define (bi! core i) (cinp! core (list-set! (cinp^ core) 1 i)))
(define (ci^ core) (list-ref (cinp^ core) 2))
;(define (ci! core i) (cinp! core (list-set! (cinp^ core) 2 i)))
(define (ao^ core) (list-ref (cout^ core) 0))
;(define (ao! core i) (cinp! core (list-set! (cout^ core) 0 i)))
(define (bo^ core) (list-ref (cout^ core) 1))
;(define (bo! core i) (cinp! core (list-set! (cout^ core) 1 i)))
(define (co^ core) (list-ref (cout^ core) 2))
;(define (co! core i) (cinp! core (list-set! (cout^ core) 2 i)))
;;universal cell node-in-matrix state
(define-immutable-record-type <uc-node>
  (mk-uc-node nid nbra nbrb nbrc ncore)
  uc-node?
  (nid nid^) (nbra nbra^) (nbrb nbrb^) (nbrc nbrc^) (ncore ncore^))
;;universal cell abstract state machine state
;;only uc-asm is mutable
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
  (let* ([nao (ao^ (ncore^ (matrix-ref node-matrix (nbra^ node))))]
         [nbo (bo^ (ncore^ (matrix-ref node-matrix (nbrb^ node))))]
         [nco (co^ (ncore^ (matrix-ref node-matrix (nbrc^ node))))]
         [core (ncore^ node)])
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
                      [(aups^) empty] [(ainp^) ups])]
         [(u (? single-standard-signal? i) empty clear clear clear)
          (set-fields asm 
                      [(ainp^) empty] [(actl^) (set-ctl inp)])]
         [(u (? (lambda (a) 
                  (and (single-standard-signal? a)
                       (or (member (list-ref a ctl) standard-signals))))
                i)
             empty clear (? non-clear? c) (? <full? b))
          (set-fields asm
                      [(ainp^) empty] [(abuf^) (write-buf inp buf)])]
         [(u empty empty clear (? non-clear? c) (? full? b))
          (process-buffer asm)]
         [(u empty (? (lambda (a) (eq? buf a)) o) clear 'one (? amp-pat? b))
          asm]
         [(u empty (? standard-signal? o) clear 'one (? amp-pat? b))
          asm]
         [(u empty empty clear 'one (? amp-pat? b))
          (set-fields asm
                      [(aout^) (set-pat 2 buf)] [(actl^) 'two])]
         [(u empty (? standard-signal? o) clear 'two (? amp-pat? b))
          asm]
         [(u empty empty clear 'two (? amp-pat? b))
          (set-fields asm
                      [(aout^) (set-pat 3 buf)] [(actl^) 'three])]
         [(u empty (? standard-signal? o) clear 'three (? amp-pat? b))
          asm]
         [(u empty empty clear 'three (? amp-pat? b))
          (set-fields asm
                      [(actl^) clear] [(abuf^) clear])]
         [(u (? special-message? i) empty clear clear clear)
          (process-special-message asm)]
         [(u empty empty (? non-clear? s) clear clear)
          (process-self-mailbox asm)]
         [_ 'halt] ;;for diagnostic purposes only
         ))]         
    [_ 'halt] ;;for diagnostic purposes only
))

(define-values (<full? full?)
  (values
   (lambda (b) (< (length b) max-buffer-length))
   (lambda (b) (eq? (length b) max-buffer-length))))
  
(define-values (amp-pat? set-pat)
  (values
   (lambda (b)
     (member b amplification-patterns))
   (lambda (n b)
     (map (lambda (a) (list-ref b (- n 1))) '(_ _ _)))))         

(define-values (clear? non-clear?)
  (values
   (lambda (c) (eq? c clear))
   (lambda (nc) (not (clear? nc)))))
(define-values (empty? non-empty?)
  (values
   (lambda (e) (eq? e empty))
   (lambda (ne) (not (empty? ne)))))
(define-values (once? count-filter? count-filter)
  (values
   (lambda (f ls) (count-filter? 1 f ls))
   (lambda (n f ls) (eq? n (count-filter f ls)))
   (lambda (f ls) (length (filter f ls)))))

(define-values (single-standard-signal? standard-signal?) 
  (values
   (lambda (i) 
     (once? standard-signal? i))
   (lambda (i)
     (and (non-empty? i)
          (and-map (lambda (a) (or (eq? a 1) (eq? a 0) (eq? a '_))) i)))))
(define (stem-init? inp) 
  (once? (lambda (a) (and (non-empty? a) (eq? a 'stem-init))) inp))
(define (special-message? i) 
  (count-filter? 1 (lambda (a) (member a special-messages)) i))
(define-values (true? false?)
  (values
   (lambda (val) (eq? val #t))
   (lambda (val) (eq? val #t))))
(define (write-buf inp buf) 
  (let ([i (car (filter (lambda (a) (member a (list 0 1))) inp))])
    (append buf (list i))))
(define-values (process-buffer bin->dec self-target?)
  (values
   (lambda (asm)
     (letrec ([buf (abuf^ asm)] 
              [tar (bin->dec (list-head buf 2))] 
              [cmd (list-ref (append special-messages standard-signals)
                             (list-tail buf 2))]
              [tmp '(_ _ _)])
       (cond
        [(eq? cmd '(1 1 1)) ;;logical amplifier
         (let ([pat (append tar (list 1))])
           (set-fields asm
                       [(ainp^) (list (car pat) (car pat) (car pat))] 
                       [(actl^) 'one] [(abuf^) pat]))]
        [(self-target? tar)
         (set-fields asm
                     [(asmb^) cmd] [(actl^) clear]
                     [(abuf^) clear])]
        [(not (self-target? tar))
         (set-fields asm
                     [(aout^) (list-set! tmp tar cmd)] 
                     [(actl^) clear] [(abuf^) clear])])))
   (lambda (bls)
     (let aux ([b bls] [e (- (length bls) 1)] [acc 0])
       (cond
        [(null? b) acc]
        [else (aux (cdr b) (- e 1) (+ acc (* (car b) (expt 2 e))))])))
   (lambda (t) (eq? t self-target-index))))
   
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
  (let* ([sms (asmb^ asm)] [buf (abuf^ asm)])
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

;;run tests
(define (run-tests) (begin 
                      (load "tests-asmsim.scm")
                      (tests)))
