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

(define (get? field cell)
  (list-ref cell (list-index universal-cell-fsm-prototype field)))

(define (clear? ls) (and-map zero? ls))
(define (present? ls) (and (not (empty? ls)) (not (clear? ls))))
(define empty? null?)

(define (mk-uc-fsm node-index node-list)
  (let* ([node (node-list-ref node-index node-list)]
         [nao (ao (nbra node-index node-list))]
         [nbo (bo (nbrb node-index node-list))]
         [nco (co (nbrc node-index node-list))])
    (append node (list nao nbo nco))))

(define (activate cell) (cons 'check-input cell))

;;SMB can apply regardless of output status.

(define (check-input cell)
  (let ([input (list (get? 'smb cell) 
                     (get? 'ai cell) (get? 'bi cell) (get? 'ci cell))])
    (cond
     [(clear? input) (cons 'collect-input cell)]
     [(present? input) (cons 'check-output cell)])))

(define (collect-input cell)
  (let ([new-input (list (get? 'nao cell) (get? 'nbo cell) (get? 'nco cell))])
    (cond
     [(clear? new-input) (cons 'end-activation cell)]
     [(present? new-input) (cons 'check-output cell)])))
                         
(define (check-output cell)
  (let ([output (list (get? 'ao cell) (get? 'bo cell) (get? 'co cell))])
    (cond
     [(clear? output) (append 'classify-input cell)]
     [(present output) (cons 'end-activation cell)])))

;we have some actionable input, and output is
(define (classify-input cell)
  (let ([non-smb-input
         (list (get? 'ai cell) (get? 'bi cell) (get? 'ci cell))])
    (cond
     [(and (clear? (get? 'smb cell)) (clear? 
      
    
    
    
