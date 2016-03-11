;;semsim.scm
;;jpt4
;;Reference implementation and simulator of PRC Universal Cell semantics.
;;UTC20160308
;;Guile Scheme v2.2

;;universal cell core state
(define (uc-core rol mem ai bi ci ao bo co hig buf)
  (define state (list rol mem ai bi ci ao bo co hig buf))
  (define (self msg . arg)
    (case msg
      [(state?) state] [(rol?) (list-ref state 0)] [(mem?) (list-ref state 1)]
      [(ai?) (list-ref state 2)] [(bi?) (list-ref state 3)] 
      [(ci?) (list-ref state 4)] 
      [(ao?) (list-ref state 5)] [(bo?) (list-ref state bo] [(co?) co]
      [(hig?) hig] [(buf?) buf]
      [(state!) (set! state (car arg))] [(rol!) (list-set! state 0 (car arg))]
      [(mem!) (list-set! state 1 (car arg))
      ))
  self
)

;;functional equivalents
(define (mk-uc-core rol mem ai bi ci ao bo co hig buf)
  (list rol mem ai bi ci ao bo co hig buf))


;;universal cell with rlem 3-453 behavior
(define (uc-3453 rol mem ai bi ci ao bo co hig buf)
  (define core-state (uc-core rol mem ai bi ci ao bo co hig buf))
  (define (self msg . arg)
    (cond
     [(member msg '(state? rol? mem? ai? bi? ci? ao? bo? co? hig? buf? 
                    state! rol! mem! ai! bi! ci! ao! bo! co! hig! buf!))
      (core-state msg . arg)]
      ))
  self
)  

;;constants
(define standard-signal 1)
(define max-buffer-length 5)
(define special-messages 
  '(stem-init wire-r-init wire-l-init proc-r-init proc-l-init write-buf-zero
              write-buf-one))

;;utilities
(define (cell-list-ref cell-list index) 
  (if (equal? index 'p) 
      (car cell-list)
      (list-ref (cdr cell-list) index)))
(define (dispnl* txt . res)
  (cond
   [(pair? txt) (begin (display (car txt)) (newline) (dispnl* (cdr txt)))]
   [(not (null? txt)) (begin (display txt) (newline) (dispnl* res))]))

;;directions
(define (west-index i) (- i 1))
(define (east-index i) (+ i 1))
(define (north-east-index i) (+ i 1))
(define (south-west-index i) (- i 1))
(define (north-west-index i cols) (- (+ i cols) 1))
(define (south-east-index i cols) (+ (- i cols) 1))

#|
(define (matrix size type)
  (cell-list))

(define (update-cell cell-id matrix)
  (let ([cell (cell-list-ref cell-id matrix)]
        [nbra (
        
|#

;;test suite
(define defaultcore (uc-core 'stem 0 0 0 0 0 0 0 '(_ _ _) '(_ _ _ _ _)))
(define tstcore defaultcore)
