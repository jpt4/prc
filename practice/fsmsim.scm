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

;;;constructors
(define (mk-uc-node r m h b s ai bi ci ao bo co)
  (list r m h b s ai bi ci ao bo co))
(define (mk-uc-fsm node-index node-list)
  (let* ([node (node-list-ref node-index node-list)]
         [nao (ao (nbra node-index node-list))]
         [nbo (bo (nbrb node-index node-list))]
         [nco (co (nbrc node-index node-list))])
    (append node (list nao nbo nco))))

;;;get
(define (peek cell field . s)
  (if (null? s)
      (list-ref cell (list-index universal-cell-fsm-prototype field))
      (peek* cell (cons field s))))
(define (peek* cell fields)
  (if (pair? fields)
      (map (lambda (e) 
             (list-ref cell (list-index universal-cell-fsm-prototype e)))
           fields)
      (peek cell fields)))
;;;set  
(define (poke cell field val)
  (let ([pivot (list-index universal-cell-fsm-prototype field)])
    (append (list-head cell pivot) (list val) (list-tail cell (+ 1 pivot)))))
(define (poke* cell fvls)
  (if (eq? (length fvls) 1)
      (poke cell (car fvls) (cadr fvls))
      (map (lambda (e) (if (eq? (car e) (car fvls)) (cadr fvls) (cadr e)))
           (zip universal-cell-fsm-prototype cell))))      
;;;input predicates
(define (clear? ls) (and-map zero? ls))
(define (present? ls) (and (not (empty? ls)) (not (clear? ls))))
(define empty? null?)
(define (special-message? mls) 
  (cond
   [(pair? mls)
    (and-map 
     (lambda (m) (exactly? 1 (lambda (l) (member m l)) special-messages))
     mls)]
   [
    (and-map (lambda (e) (not (eq? 
      (member m special-messages)))
;;;utilities
(define (exactly? num fn ls)
  (let ([fil (filter fn ls)])
    (if (eq? (length fil) num)
        fil
        #f)))
(define (zip lsa lsb)
  (map (lambda (e) (list (list-ref lsa e)
                         (list-ref lsb e)))
       (iota (length lsa))))

;;;annotated cell->cell functions
(define (activate cell) (cons 'check-mail cell))
;;SMB can apply regardless of output status; mail is non-blocking input
(define (check-mail cell)
  (let ([mail (peek 'smb cell)])
    (cond
     [(empty? mail) (cons 'check-input cell)]
     [(special-message? mail) (cons 'process-special-message cell)])))

(define (check-input cell)
  (let ([input (list (peek 'ai cell) (peek 'bi cell) (peek 'ci cell))])
    (cond
     [(clear? input) (cons 'collect-input cell)]
     [(present? input) (cons 'classify-input cell)])))

(define (collect-input cell)
  (let* ([new-input (list (peek 'nao cell) (peek 'nbo cell) (peek 'nco cell))]
         [new-cell (poke* cell '((nao 0) (nbo 0) (nco 0)))]
    (cond
     [(clear? new-input) (cons 'end-activation new-cell)]
     [(present? new-input) (cons 'classify-input new-cell)])))
                         
;;we have some actionable non-smb input
(define (classify-input cell)
  (let ([input (list (peek 'ai cell) (peek 'bi cell) (peek 'ci cell))])
    (cond
     [(special-message? input) 
      (cons 'process-special-message cell)] ;output irrelevant
     [(standard-signal? input) (cons 'check-output cell)]

(define (check-output cell)
  (let ([output (list (peek 'ao cell) (peek 'bo cell) (peek 'co cell))])
    (cond
     [(clear? output) (cons 'process-standard-signal cell)] 
     [(present? output) ;standard input blocks if output present
      (cons 'end-activation cell)])))
      
    
    
    
