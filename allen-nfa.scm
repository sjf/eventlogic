;; allen relations on regular languages using superposition

(module
 allen-nfa
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
	 (situation "situation.scm"))
 (main main-allen))
 

(define nfa-empty-plus
  (nfa-plus (nfa-for-one-symbol empty-snapshot)))

(define (equal nfa1 nfa2 snapshot-seq)
  ; L1
  ; L2
  (let ((equal-dfa (superposition (nfa->dfa nfa1) (nfa->dfa nfa2))))
    (run-dfa equal-dfa snapshot-seq)))

(define (before nfa1 nfa2 snapshot-seq)
  ; L1 []+ L2
  (let* ((before-nfa 
	 (nfa-concat nfa1 (nfa-concat nfa-empty-plus nfa2)))
	(before-dfa (nfa->dfa before-nfa)))
    (run-dfa before-dfa snapshot-seq)))
(define (after nfa1 nfa2 snapshot-seq) (before nfa2 nfa1 snapshot-seq))

(define (meets nfa1 nfa2 snapshot-seq)
  ; L1 L2
  (let ((meets-nfa (nfa->dfa (nfa-concat nfa1 nfa2))))
    (run-dfa meets-nfa snapshot-seq)))
(define (met-by nfa1 nfa2 snapshot-seq) (meets nfa2 nfa1 snapshot-seq))

(define (overlaps nfa1 nfa2 snapshot-seq)
  ; L1----- []+
  ; []+ L2-----
  (let* ((nfaA (nfa-concat nfa1 nfa-empty-plus))
	 (nfaB (nfa-concat nfa-empty-plus nfa2))
	 (overlaps-dfa (superposition (nfa->dfa nfaA) (nfa->dfa nfaB))))
    ;(print overlaps-dfa)
    (run-dfa overlaps-dfa snapshot-seq)))
(define (overlapped-by nfa1 nfa2 snapshot-seq) (overlaps nfa2 nfa1 snapshot-seq))

(define (contains nfa1 nfa2 snapshot-seq)
  ;----L1----
  ;[]+-L2-[]+ 
  (let* ((nfa3 (nfa-concat nfa-empty-plus
			   (nfa-concat nfa2 nfa-empty-plus)))
	 (contains-dfa (superposition (nfa->dfa nfa1) (nfa->dfa nfa3))))
    (run-dfa contains-dfa snapshot-seq)))
(define (during nfa1 nfa2 snapshot-seq) (contains nfa2 nfa1 snapshot-seq))

(define (starts nfa1 nfa2 snapshot-seq)
  ; L1-- []+
  ; L2------
  (let* ((nfa3 (nfa-concat nfa1 nfa-empty-plus))
	(starts-dfa (superposition (nfa->dfa nfa3) (nfa->dfa nfa2))))
    (run-dfa starts-dfa snapshot-seq)))
(define (started-by nfa1 nfa2 snapshot-seq) (starts nfa2 nfa1 snapshot-seq))

(define (ends nfa1 nfa2 snapshot-seq)
  ; []+ --L1
  ; L2------
  (let* ((nfa3 (nfa-concat nfa-empty-plus nfa1))
	 (ends-dfa (superposition (nfa->dfa nfa3) (nfa->dfa nfa2))))
    (run-dfa ends-dfa snapshot-seq)))
(define (ended-by nfa1 nfa2 snapshot-seq) (ends nfa2 nfa2 snapshot-seq))


;; the relation procedures (unquoted) plus their names
(define relations 
  `((,equal equals) (,before before)
    (,after after) (,meets meets) (,met-by met-by) 
    (,overlaps overlaps) (,overlapped-by overlapped-by)
    (,contains contains) (,during during) (,started-by started-by)
    (,ends ends) (,ended-by ended-by)))

(define (main-allen argv)
;do some tests
(if #f (begin 
(let* ((r1 "[a].[a].[a].[a]")
       (r2 "[b].[b]")
       (str (str->snapshot-seq "[a][a b][a b][a][c]"))
       (nfa1 (parse-tree->nfa (str->parse-tree r1)))
       (nfa2 (parse-tree->nfa (str->parse-tree r2))))
  
  (print (contains nfa1 nfa2 str)))
))

(let loop ()
  (let* ((e1 (begin (print "event 1: ") (regex->nfa (read-line))))
	 (e2 (begin (print "event 2: ") (regex->nfa (read-line))))
	 (str (begin (print "input str: ") (str->snapshot-seq (read-line)))))
    (map (lambda (f) (print (cadr f) " " ((car f) e1 e2 str)))
	 relations))
  (loop))

(exit)
(let* ((e1 (regex->nfa (car (get-args))))
       (e2 (regex->nfa (cadr (get-args))))
       (str (str->snapshot-seq (caddr (get-args)))))
  (map (lambda (f) (print f " " (f e1 e2 str)))
       (list overlaps)))
       ;(list equal before after meets met-by overlaps overlapped-by contains
	;     during starts started-by ends ended-by)))

(exit)

)
