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
          'write-buf-zero 'write-buf-one))

;;universal cell core (aka cell) state
(define-immutable-record-type <uc-core>
  (mk-uc-core rol mem hig buf smb ai bi ci ao bo co)
  uc-core?
  (rol rol? rol!) (mem mem? mem!) (hig hig? hig!) 
  (buf buf? buf!) (smb smb? smb!)
  (ai ai? ai!) (bi bi? bi!) (ci ci? ci!) 
  (ao ao? ao!) (bo bo? bo!) (co co? co!))
;;uc node-in-matrix state
(define-immutable-record-type <uc-node>
  (mk-uc-node nid nbra nbrb nbrc uc-core)
  uc-node?
  (nid nid? nid!) (nbra nbra? nbra!) (nbrb nbrb? nbrb!) (nbrc nbrc? nbrc!)
  (uc-core node-core? node-core!))
;;uc abstract state machine state
(define-immutable-record-type <uc-asm>
  (make-uc-asm sta  nao nbo nco uc-core)
  uc-asm?
  (sta sta? sta!) (nao nao? nao!) (nbo nbo? nbo!) (nco nco? nco!)
  (uc-core asm-core? asm-core!))

;;constructors
(define (mk-uc-asm node node-matrix)
  (let* ([core (core? node)]
         [nao (ao? (matrix-ref node-matrix (nbra? node)))]
         [nbo (bo? (matrix-ref node-matrix (nbrb? node)))]
         [nco (co? (matrix-ref node-matrix (nbrc? node)))])
    (make-uc-asm 'init nao nbo nco node)))

;;state machine interpretation
(define (step asm)
  (match asm
    [($ <uc-asm> 'activate nao nbo nco core)
     (step (asm-activate asm))]
    [_ 'halt]
    ))

(define (asm-activate asm) (sta! asm 'init))  

;;uc matrix
(define (matrix-ref matrix index) (hash-ref matrix index))

;;;test suite
(define uc-core-prototype
  (mk-uc-core 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co))
(define uc-node-prototype
  (mk-uc-node 'nid 'nbra 'nbrb 'nbrc uc-core-prototype))
(define uc-asm-prototype
  (make-uc-asm 'sta 'nao 'nbo 'nco uc-core-prototype))

(define (tests) 
  (begin
    (display uc-core-prototype) (newline)
    (display uc-node-prototype) (newline)
    (display uc-asm-prototype) (newline)
    (display (set-fields uc-core-prototype
                         [(hig?) '(0 0 1)]
                         [(buf?) '(1 1 1 1)]))
    (newline)
    (display (set-fields uc-node-prototype 
                         [(nbrc?) 3] 
                         [(node-core? rol?) 'stem]))
    (newline)
    (display (set-fields uc-asm-prototype 
                         [(asm-core? mem?) '1]
                         [(sta?) 'init]))
    (newline)
    (display (asm-activate (sta! uc-asm-prototype 'activate)))
    (newline)
    (display (step (sta! uc-asm-prototype 'activate)))
    (newline)
    ))

