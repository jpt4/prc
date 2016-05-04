;;asmsim.scm
;;jpt4
;;An abstract state machine simulation model for Universal Cell matrices.
;;UTC20160502
;;Guike Scheme v.2.0+

;;srfi-9 (make-record-type)
(use-modules (srfi srfi-9 gnu))
;;pattern matching module (match)
(use-modules (ice-9 match))

;;;universal cell
;;constants
(define standard-signal 1) (define max-buffer-length 5)
(define special-messages 
  (vector 'stem-init 'wire-r-init 'wire-l-init 'proc-r-init 'proc-l-init 
          'write-buf-zero 'write-buf-one))

;;uc node state
(define-immutable-record-type <uc-node>
  (mk-uc-node rol mem hig buf smb ai bi ci ao bo co)
  uc-node?
  (rol rol? rol!) (mem mem? mem!) (hig hig? hig!) 
  (buf buf? buf!) (smb smb? smb!)
  (ai ai? ai!) (bi bi? bi!) (ci ci? ci!) 
  (ao ao? ao!) (bo bo? bo!) (co co? co!))
;;uc abstract state machine state
(define-immutable-record-type <uc-asm>
  (make-uc-asm sta uc-node nao nbo nco)
  uc-asm?
  (sta sta? sta!) (uc-node node? node!)
  (nao nao? nao!) (nbo nbo? nbo!) (nco nco? nco!))
;;constructors
(define (mk-uc-asm node-index node-matrix)
  (let* ([node (node-list-ref node-matrix node-index)]
         [nao (ao? (nbra node-index node-matrix))]
         [nbo (bo? (nbrb node-index node-matrix))]
         [nco (co? (nbrc node-index node-matrix))])
    (make-uc-asm 'init node nao nbo nco)))
         

;;;test suite
(define uc-node-prototype
  (mk-uc-node 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co))
(define uc-asm-prototype
  (make-uc-asm 'sta uc-node-prototype 'nao 'nbo 'nco))

(define (tests) 
  (begin
    (display uc-node-prototype) (newline)
    (display uc-asm-prototype) (newline)
    (set-fields uc-asm-prototype ((node? rol?) 'stem) ((sta?) 'init)) 
    (newline)
    ))

