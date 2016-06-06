(use-modules (srfi srfi-9 gnu))
(use-modules (srfi srfi-9))
(use-modules (ice-9 match))
(define-record-type <person>
  (make-person name friends)
  person?
  (name person-name) (friends person-friends))

(define alice (make-person 'alice 'bob))
(display (person-friends alice))
(newline)

(define (friends? p)
  (match p
         [($ <person> name friends) friends]
         [_ #f]))

(display (friends? alice))
(newline)
