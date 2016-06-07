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
(define special-messages 
  (vector 'stem-init 'wire-r-init 'wire-l-init 'proc-r-init 'proc-l-init 
          'zero 'one))

;;universal cell core (aka cell) state
(define-immutable-record-type <uc-core>
  (mk-uc-core rol mem hig buf smb inp out)
  uc-core?
  (rol rol? rol!) (mem mem? mem!) (hig hig? hig!) 
  (buf buf? buf!) (smb smb? smb!)
  (inp inp? inp!) (out out? out!))
(define (ai? core) (list-ref (inp? core) 0))
(define (ai! core i) (inp! core (list-set! (inp? core) 0 i)))
(define (bi? core) (list-ref (inp? core) 1))
(define (bi! core i) (inp! core (list-set! (inp? core) 1 i)))
(define (ci? core) (list-ref (inp? core) 2))
(define (ci! core i) (inp! core (list-set! (inp? core) 2 i)))
(define (ao? core) (list-ref (out? core) 0))
(define (ao! core i) (inp! core (list-set! (out? core) 0 i)))
(define (bo? core) (list-ref (out? core) 1))
(define (bo! core i) (inp! core (list-set! (out? core) 1 i)))
(define (co? core) (list-ref (out? core) 2))
(define (co! core i) (inp! core (list-set! (out? core) 2 i)))
;;uc node-in-matrix state
(define-immutable-record-type <uc-node>
  (mk-uc-node nid nbra nbrb nbrc uc-core)
  uc-node?
  (nid nid? nid!) (nbra nbra? nbra!) (nbrb nbrb? nbrb!) (nbrc nbrc? nbrc!)
  (uc-core node-core? node-core!))
;;uc abstract state machine state
(define-immutable-record-type <uc-asm>
  (mk-uc-asm sta pre uc-core)
  uc-asm?
  (sta sta? sta!) (pre pre? pre!)
  (uc-core asm-core? asm-core!))
(define (nao? asm) (list-ref (pre? asm) 0))
(define (nao! asm v) (pre! asm (list-set! (pre? asm) 0 v)))
(define (nbo? asm) (list-ref (pre? asm) 1))
(define (nbo! asm v) (pre! asm (list-set! (pre? asm) 1 v)))
(define (nco? asm) (list-ref (pre? asm) 2))
(define (nco! asm v) (pre! asm (list-set! (pre? asm) 2 v)))

;;constructors
(define (make-uc-asm node node-matrix)
  (let* ([core (node-core? node)]
         [nao (ao? (node-core? (matrix-ref node-matrix (nbra? node))))]
         [nbo (bo? (node-core? (matrix-ref node-matrix (nbrb? node))))]
         [nco (co? (node-core? (matrix-ref node-matrix (nbrc? node))))])
    (mk-uc-asm 'q0 (list nao nbo nco) core)))

;;state machine interpretation
(define (step asm)
  (match asm
    ;;wire standard signal
    [($ <uc-asm> 'q0 (? empty pre) ;0 0 0 -> 0 0 0
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? empty inp) (? empty out)))
     asm]
    [($ <uc-asm> 'q0 (? empty pre) ;0 0 1ne -> 0 0 1ne
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? empty inp) (? not-empty out)))
     asm]
    [($ <uc-asm> 'q0 (? empty pre) ;0 1ss 0 -> 0 0 1ss
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? standard inp) (? empty out)))
     (set-fields asm 
                 [(asm-core? inp?) (clear inp)]
                 [(asm-core? out?) (rotate inp (+ 1 mem))])]
    [($ <uc-asm> 'q0 (? empty pre) ;0 1ss 1ne -> 0 1ss 1ne
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? standard inp) (? not-empty out)))
     asm]
    [($ <uc-asm> 'q0 (? not-empty pre) ;1ne 0 0 -> 0 1ne 0
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? empty inp) (? empty out)))
     (set-fields asm 
                 [(pre?) (clear pre)]
                 [(asm-core? inp?) pre])]
    [($ <uc-asm> 'q0 (? not-empty pre) ;1ne 0 1ne -> 0 1ne 1ne 
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? empty inp) (? not-empty out)))
     (set-fields asm 
                 [(pre?) (clear pre)]
                 [(asm-core? inp?) pre])]
    [($ <uc-asm> 'q0 (? not-empty pre) ;1ne 1ss 0 -> 1ne 0 1ss
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? standard inp) (? empty out)))
     (set-fields asm 
                 [(asm-core? inp?) (clear inp)]
                 [(asm-core? out?) (rotate inp (+ 1 mem))])]
    [($ <uc-asm> 'q0 (? not-empty pre) ;1ne 1ss 1ne -> 1ne 1ss 1ne
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? standard inp) (? not-empty out)))
     asm]
    ;;wire special signal
    [($ <uc-asm> 'q0 pre ;wire: 0/1 1sm 0/1 -> stem: 0 0 0
        ($ <uc-core> 'wire mem (? empty hig) (? empty buf) (? empty smb)
           (? stem-init? inp) out))
     (set-fields asm
                 [(asm-core? rol?) 'stem] [(asm-core? mem?) 0] 
                 [(asm-core? inp?) (clear inp)] [(asm-core? out?) (clear out)]
                 )]
    [_ 'halt]
    ))

(define (empty v) (and-map (lambda (l) (eq? l '_)) v))
(define (not-empty v) (not (empty v)))
(define (standard i) 
  (and (not (eq? '(_ _ _) i))
       (map (lambda (a) (or (eq? a 1) (eq? a 0) (eq? a '_))) i)))
(define (special? i) (exactly? 1 (lambda (a) (member? a special-messages)) i))
(define (stem-init? i) (exactly? 1 (lambda (a) (eq? a 'stem-init)) i))
(define (clear f) (map (lambda (a) '_) f))
(define (rotate l n) (list-rotate l n))
(define (list-rotate ls num)
	(let* ([snum (modulo num (length ls))] ;sanitized shift value
				 [new-head (list-tail ls (- (length ls) snum))]
				 [new-tail (list-head ls (- (length ls) snum))])
		(append new-head new-tail)))

;;uc matrix
(define (matrix-ref matrix index) (hash-ref matrix index))

;;;test suite
(define uc-core-prototype
  (mk-uc-core 'rol 'mem 'hig 'buf 'smb 'inp 'out))
(define uc-node-prototype
  (mk-uc-node 'nid 'nbra 'nbrb 'nbrc uc-core-prototype))
(define uc-asm-prototype
  (mk-uc-asm 'sta (list 'nao 'nbo 'nco) uc-core-prototype))

(define (dispnl t) (begin (display t) (newline)))
(define (dispnl* tls) 
  (cond
   [(and (pair? tls) (pair? (cdr tls))) 
    (begin (dispnl* (car tls)) (for-each dispnl* (cdr tls)))]
   [(and (pair? tls) (null? (cdr tls))) (dispnl* (car tls))]
   [else (dispnl tls)]))
   

(define (tests) 
  (begin
    (dispnl* 
     (list 
      uc-core-prototype  uc-node-prototype uc-asm-prototype
      (set-fields uc-core-prototype
                  [(hig?) '(0 0 1)]
                  [(buf?) '(1 1 1 1)])
      (set-fields uc-node-prototype 
                  [(nbrc?) 3]
                  [(node-core? rol?) 'stem])
      (set-fields uc-asm-prototype 
                  [(asm-core? mem?) '1]
                  [(sta?) 'init])
      "0 0 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 0 '() '() '() '(_ _ _) '(_ _ _)))])
        (list asm (step asm)))
      "0 0 1"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 0 '() '() '() '(_ _ _) '(1 0 1)))])
        (list asm (step asm)))
      "0 1sr 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 0 '() '() '() '(1 0 1) '(_ _ _)))])
        (list asm (step asm)))
      "0 1sl 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 1 '() '() '() '(1 0 1) '(_ _ _)))])
        (list asm (step asm)))
      "0 1ns 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 1 '() '() '() '(_ _ _) '(_ _ _)))])
        (list asm (step asm)))
      "0 1 1"
      (let ([asm (mk-uc-asm 
                  'q0 '(_ _ _) (mk-uc-core 
                                'wire 0 '() '() '() '(1 0 1) '(0 0 _)))])
        (list asm (step asm)))
      "1 0 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 0 '() '() '() '(_ _ _) '(_ _ _)))])
        (list asm (step asm)))
      "1 0 1"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 0 '() '() '() '(_ _ _) '(1 1 1)))])
        (list asm (step asm)))
      "1 1sr 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core
                                'wire 0 '() '() '() '(_ 1 1) '(_ _ _)))])
        (list asm (step asm)))
      "1 1sl 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 1 '() '() '() '(_ 1 1) '(_ _ _)))])
        (list asm (step asm)))
      "1 1ns 0"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 1 '() '() '() '(_ _ _) '(_ _ _)))])
        (list asm (step asm)))
      "1 1 1"
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 0 '() '() '() '(1 1 1) '(0 0 0)))])
        (list asm (step asm)))
      (let ([asm (mk-uc-asm 
                  'q0 '(1 _ 0) (mk-uc-core 
                                'wire 0 '() '() '() '(1 stem-init 1) 
                                '(0 1 0)))])
        (list asm (step asm)))      
      ))           
    ))
