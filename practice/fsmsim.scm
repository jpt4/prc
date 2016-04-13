;;fsmsim.scm
;;jpt4
;;PRC Universal Cell semantics implemented via Finite State Machines, with
;;the goal of provable correctness.
;;UTC20160410
;;Guile Scheme v2.0+

;;;constants
(define standard-signal 1)
(define max-buffer-length 5)
(define special-messages 
  '(stem-init wire-r-init wire-l-init proc-r-init proc-l-init write-buf-zero
              write-buf-one))
(define universal-cell-node-prototype
  (list 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co))
(define universal-cell-fsm-prototype
  (list 'rol 'mem 'hig 'buf 'smb 'ai 'bi 'ci 'ao 'bo 'co 'nao 'nbo 'nco))

(define (mk-uc-node r m h b s ai bi ci ao bo co)
  (list r m h b s ai bi ci ao bo co))

(define (mk-uc-fsm node-index node-list)
  (let* ([node (node-list-ref node-index node-list)]
         [nao (ao (nbra node-index node-list))]
         [nbo (bo (nbrb node-index node-list))]
         [nco (co (nbrc node-index node-list))])
    (append node (list nao nbo nco))))
