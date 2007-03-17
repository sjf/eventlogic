;; allen relations on regular languages using superposition
(module
 allen-nfa
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
	 (graph "graph.scm")
	 (snapshots "snapshots.scm"))
 (export 
  (equal a b)
  (before a b)
  (after a b)
  (meets a b)
  (met-by a b)
  (overlaps a b)
  (overlapped-by a b)
  (contains a b)
  (during a b)
  (starts a b)
  (started-by a b)
  (ends a b)
  (ended-by a b))
 (main main-allen))
 

;; These functions return a dfa 
;; for each of the 13 allen relations.
;; They take two nfas as arguments and return
;; the dfa that accept when two strings from the
;; nfas occur in that relation to each other.

(define (equal nfa1 nfa2)
  ;   L1
  ; & L2
  (superposition (nfa->dfa nfa1) (nfa->dfa nfa2)))

(define (before nfa1 nfa2)
  ; L1.[]+.L2
  (nfa->dfa (nfa-concat nfa1 (nfa-concat (nfa-empty-plus) nfa2))))

(define (after nfa1 nfa2) (before nfa2 nfa1))

(define (meets nfa1 nfa2)
  ; L1.L2
  (nfa->dfa (nfa-concat nfa1 nfa2)))
(define (met-by nfa1 nfa2) (meets nfa2 nfa1))

(define (overlaps nfa1 nfa2)
  ;   L1----- []+
  ; & []+ L2-----   less  L1.L2  less L1.[]+.L2
  (let* ((nfaA (nfa-concat nfa1 (nfa-empty-plus)))
	 (nfaB (nfa-concat (nfa-empty-plus) nfa2))
	 (superposAB (superposition (nfa->dfa nfaA) (nfa->dfa nfaB)))
	 (meet-dfa (meets nfa1 nfa2))
	 (before-dfa (before nfa1 nfa2)))
    (let ((d
	   (dfa-less (dfa-less superposAB meet-dfa)
		     before-dfa)))
      (show-graph (graph superposAB))
      (show-graph (graph d))
      d)))


(define (overlapped-by nfa1 nfa2) (overlaps nfa2 nfa1))

(define (contains nfa1 nfa2)
  ;   ----L1----   
  ; & []+-L2-[]+ 
  (let* ((nfa3 (nfa-concat (nfa-empty-plus)
			   (nfa-concat nfa2 (nfa-empty-plus)))))
    (superposition (nfa->dfa nfa1) (nfa->dfa nfa3))))
	 

(define (during nfa1 nfa2) (contains nfa2 nfa1))

(define (starts nfa1 nfa2)
  ;   L1-- []+
  ; & L2------
  (superposition (nfa->dfa (nfa-concat nfa1 (nfa-empty-plus))) (nfa->dfa nfa2)))
(define (started-by nfa1 nfa2) (starts nfa2 nfa1))

(define (ends nfa1 nfa2)
  ;   []+ --L1
  ; & L2------
  (superposition (nfa->dfa (nfa-concat (nfa-empty-plus) nfa1)) (nfa->dfa nfa2)))
(define (ended-by nfa1 nfa2) (ends nfa2 nfa1))


;; the relation procedures (unquoted) plus their names
(define relations 
  `((,equal equals) (,before before)
    (,after after) (,meets meets) (,met-by met-by) 
    (,overlaps overlaps) (,overlapped-by overlapped-by)
    (,contains contains) (,during during) (,starts starts)    
    (,started-by started-by) (,ends ends) (,ended-by ended-by)))

(define (run-all-relations e1 e2 str)
  (run-relations e1 e2 str relations))

(define (run-relations e1 e2 str relations)
  (map (lambda (f) 
	 (let ((dfa1 ((car f) e1 e2)))
	    (run-dfa dfa1 str)))
       relations))  
   

(define (main-allen argv)
  ;do some tests
;;   (let* ((r1 "[1].[2]")
;; 	 (r2 "[2].[3]")
;; 	 (str (str->snapshot-seq "[1][2][3][4]"))
;; 	 (nfa1 (parse-tree->nfa (str->parse-tree r1)))
;; 	 (nfa2 (parse-tree->nfa (str->parse-tree r2))))
  (let ((l1 (read-line))
	(l2 (read-line)))
    (print l1 " " l2)
    (let ((e1 (regex->nfa l1))
	  (e2 (regex->nfa l2)))
      (let loop ()
	  (let ((l (read-line)))
	    (if (not (equal? l #eof-object))
		(let ((str (str->snapshot-seq l)))
		  ;; for each allen relation build the dfa and run it on the input
		  ;; and print the results
		  (print "---------------------------------")
		  (print str)
		  (map (lambda (f) (print (cadr f) " " (run-dfa ((car f) e1 e2) str)))
		       relations)
		  (loop)))))))
    
  (exit)
  (let* ((e1 (regex->nfa (car argv)))
	 (e2 (regex->nfa (cadr argv)))
	 (str (str->snapshot-seq (caddr argv))))
    (map (lambda (f) (print f " " (f e1 e2 str)))
	 (list overlaps)))
       ;(list equal before after meets met-by overlaps overlapped-by contains
	;     during starts started-by ends ended-by)))

  (exit)
)
