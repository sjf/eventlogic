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
  ; L1
  ; L2
  (superposition (nfa->dfa nfa1) (nfa->dfa nfa2)))

(define (before nfa1 nfa2)
  ; L1 []+ L2
  (nfa->dfa (nfa-concat nfa1 (nfa-concat (nfa-empty-plus) nfa2))))

(define (after nfa1 nfa2) (before nfa2 nfa1))

(define (meets nfa1 nfa2)
  ; L1 L2
  (nfa->dfa (nfa-concat nfa1 nfa2)))
(define (met-by nfa1 nfa2) (meets nfa2 nfa1))

(define (overlaps nfa1 nfa2)
  ; L1----- []+
  ; []+ L2-----
  (let* ((nfaA (nfa-concat nfa1 (nfa-empty-plus)))
	 (nfaB (nfa-concat (nfa-empty-plus) nfa2)))    
    (superposition (nfa->dfa nfaA) (nfa->dfa nfaB))))
(define (overlapped-by nfa1 nfa2) (overlaps nfa2 nfa1))

(define (contains nfa1 nfa2)
  ;----L1----
  ;[]+-L2-[]+ 
  (let* ((nfa3 (nfa-concat (nfa-empty-plus)
			   (nfa-concat nfa2 (nfa-empty-plus)))))
    (print-nfa nfa1)
    (print-nfa nfa3)
    (let* ((dfa3 (nfa->dfa nfa3))
	   (dfa1 (nfa->dfa nfa1))
	   (dfa4
	    (superposition dfa1 dfa3)))
      (print-dfa dfa1)
      (print-dfa dfa3)
      (print-dfa dfa4)
      dfa4)))

(define (during nfa1 nfa2) (contains nfa2 nfa1))

(define (starts nfa1 nfa2)
  ; L1-- []+
  ; L2------
  (superposition (nfa->dfa (nfa-concat nfa1 (nfa-empty-plus))) (nfa->dfa nfa2)))
(define (started-by nfa1 nfa2) (starts nfa2 nfa1))

(define (ends nfa1 nfa2)
  ; []+ --L1
  ; L2------
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
  (map print-nfa (list e1 e2))
  (map (lambda (f) 
	 (print (cadr f) " ") 
	 (let ((dfa1 ((car f) e1 e2)))
	   (read)
	   (show-graph (graph dfa1))
	   (read)
	   (print (run-dfa dfa1 str))))
       relations))  
   

(define (main-allen argv)
;;   (let* ((t-dfa1 (list (list 'q0 '(s1) 'q1)
;; 		       (list 'q1 '(s2) 'q2)))
;; 	(dfa1 (dfa 'q0
;; 		   (make-transition-function t-dfa1)
;; 		   t-dfa1
;; 		   (list 'q2)
;; 		   (list '(s1) '(s2))))
;; 	(t-dfa2 '((qA (sE) qB)
;; 		  (qB (sE) qB)
;; 		  (qB (s3) qC)
;; 		  (qC (s4) qD)
;; 		  (qD (sE) qE)
;; 		  (qE (sE) qE)))
;; 	(dfa2 (dfa 'qA
;; 		   (make-transition-function t-dfa2)
;; 		   t-dfa2
;; 		   (list 'qE)
;; 		   '((s3) (sE)))))
;;     (print-dfa dfa1)
;;     (print-dfa dfa2)
;;     (show-graph (graph dfa2))
;;     (print-dfa (superposition dfa1 dfa2))
;;     (show-graph (graph (superposition dfa1 dfa2))))
  
;;   (exit)

  ;do some tests
  (let* ((r1 "[1].[2]")
	 (r2 "[2].[3]")
	 (str (str->snapshot-seq "[1][2][3][4]"))
	 (nfa1 (parse-tree->nfa (str->parse-tree r1)))
	 (nfa2 (parse-tree->nfa (str->parse-tree r2))))

  ;----L1----
  ;[]+-L2-[]+ 
	   
    (let* ((nfa3 (nfa-concat (nfa-empty-plus)
			     (nfa-concat nfa2 (nfa-empty-plus)))))
      (print-nfa nfa1)
      (print-nfa nfa3)
      
      (let* ((dfa3 (nfa->dfa nfa3))
	     (dfa1 (nfa->dfa nfa1))
	     (dfa4
	      (superposition dfa1 dfa3)))
	(print-dfa dfa1)
	(print-dfa dfa3)
	;(show-graph (graph dfa3))
	(print-dfa dfa4)
	;(show-graph (graph dfa1))
	;(show-graph (graph dfa3))
	(show-graph (graph dfa4))
	dfa4)))

;;     (run-relations nfa1 nfa2 str
;; 		   `((,contains contains)
;; 		     (,starts starts)
;; 		     (,ends ends))))
  (exit)
  
  
  (let loop ()
    (let* ((e1 (begin (print "event 1 regex: ") (regex->nfa (read-line))))
	   (e2 (begin (print "event 2 regex: ") (regex->nfa (read-line))))
	   (str (begin (print "input sequence: ") (str->snapshot-seq (read-line)))))
      ;; for each allen relation build the dfa and run it on the input
      ;; and print the results
      (map (lambda (f) (print (cadr f) " " (run-dfa ((car f) e1 e2) str)))
	   relations))
    (loop))
  
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
