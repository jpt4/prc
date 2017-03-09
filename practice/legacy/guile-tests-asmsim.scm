;;tests-asmsim.scm
;;jpt4
;;test suite for asmsim.scm
;;UTC20160811
;;Guile Scheme v.2.0+

(load "asmsim.scm")

(define uc-core-prototype
  (mk-uc-core 'inp 'out 'smb 'rol 'mem 'ctl 'buf))
(define uc-node-prototype
  (mk-uc-node 'nid 'nbra 'nbrb 'nbrc uc-core-prototype))
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
                    [(ncore^ crol^) 'stem]
                    [(ncore^ cout^) '(1 0 1)])
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
