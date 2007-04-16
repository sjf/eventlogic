;; allen relations on regular languages using superposition
(module
 allen-nfa
; (main main-allen)
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
	 (graph "graph.scm")
	 (snapshots "snapshots.scm"))
 (export 
  (allen r a b)
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
  (ended-by a b)
  (weak-overlaps a b)))

;; the relation procedures (unquoted) plus their names
(define relations 
  `((,equal equal) (,before before)
    (,after after) (,meets meets) (,met-by met-by) 
    (,overlaps overlaps) (,overlapped-by overlapped-by)
    (,contains contains) (,during during) (,starts starts)    
    (,started-by started-by) (,ends ends) (,ended-by ended-by)))

(define (allen relation a b)
  (dump-trace)
  ;; get the procedure for this allen relation
  ;; and the create the nfa for it
  (let ((r (find-if (lambda (p) 
		      (equal? (second p) relation)) relations)))
    (if (equal? #f r)
	(error "allen" "Is not an allen relation" relation)
	; create the nfa
	((first r) a b))))
	

;; These functions return a dfa 
;; for each of the 13 allen relations.
;; They take two nfas as arguments and return
;; the nfa that accept when two strings from the
;; nfas occur in that relation to each other.

(define (equal nfa1 nfa2)
  ;   L1
  ; & L2
  (dfa->nfa (superposition (nfa->dfa nfa1) (nfa->dfa nfa2))))

(define (before nfa1 nfa2)
  ; L1.[]+.L2
  (nfa-concat nfa1 (nfa-concat (nfa-empty-plus) nfa2)))

(define (after nfa1 nfa2) (before nfa2 nfa1))

(define (meets nfa1 nfa2)
  ; L1.L2
  (nfa-concat nfa1 nfa2))
(define (met-by nfa1 nfa2) (meets nfa2 nfa1))

(define t 1)
(define (p) (print t) (set! t (+ t 1)) #t)
(define (overlaps nfa1 nfa2)
  ;   L1----- []+
  ; & []+ L2-----   less  L1.[]*.L2
  (let* ((nfaA (nfa-concat nfa1 (nfa-empty-plus)))
	 (d1 (p))
	 (nfaB (nfa-concat (nfa-empty-plus) nfa2))
	 (d2 (p))
	 (dfaA (nfa->dfa nfaA))
	 (d3 (p))
	 (dfaB (nfa->dfa nfaB))
	 (d4 (p))
	 (superposAB-dfa (superposition dfaA dfaB))
	 (d5 (p))
	 (not-overlapped-dfa (nfa->dfa (nfa-concat nfa1 (nfa-star (nfa-empty)) nfa2)))
	 (d6 (and (p) (print "dfa lessing ..")))
	 (result-dfa (dfa-less superposAB-dfa not-overlapped-dfa))
	 (d7 (and (p) (print "nfa->dfa"))))
	 
    (dfa->nfa result-dfa)))
(define (overlapped-by nfa1 nfa2) (overlaps nfa2 nfa1))

(define (contains nfa1 nfa2)
  ;;   ----L1----   
  ;; & []+-L2-[]+ 
  (let* ((nfa3 (nfa-concat (nfa-empty-plus)
			   (nfa-concat nfa2 (nfa-empty-plus)))))
    (dfa->nfa (superposition (nfa->dfa nfa1) (nfa->dfa nfa3)))))
	 

(define (during nfa1 nfa2) (contains nfa2 nfa1))

(define (starts nfa1 nfa2)
  ;;   L1-- []+
  ;; & L2------
  (dfa->nfa (superposition (nfa->dfa (nfa-concat nfa1 (nfa-empty-plus))) (nfa->dfa nfa2))))
(define (started-by nfa1 nfa2) (starts nfa2 nfa1))

(define (ends nfa1 nfa2)  
  ;;   []+ --L1
  ;; & L2------
  (dfa->nfa (superposition (nfa->dfa (nfa-concat (nfa-empty-plus) nfa1)) (nfa->dfa nfa2))))
(define (ended-by nfa1 nfa2) (ends nfa2 nfa1))


;; Any part of L1 and L2 overlap.
;; L1 does not have to appear before L2.
;; This is same as (equal or starts or
;; overlaps or during or during) as 
;; well as their inverses.
(define (weak-overlaps nfa1 nfa2)
  (nfa-or (%weak-overlaps nfa1 nfa2)
	   (%weak-overlaps nfa2 nfa1)))

(define (%weak-overlaps nfa1 nfa2)
  ;;   L1.[]*  
  ;; & []*.L2   - L1.[]*.L2
  (let* ((d (print 1))
	 (dfa1 (nfa->dfa (nfa-concat nfa1 (nfa-star (nfa-empty)))))
	 (d1 (print 2))
	 (dfa2 (nfa->dfa (nfa-concat (nfa-star (nfa-empty)) nfa2)))
	 (d2 (print 3))
	 (not-overlapped (nfa->dfa (nfa-concat nfa1 (nfa-star (nfa-empty)) nfa2)))
	 (d3 (print 4))
	 (superpos (superposition dfa1 dfa2))
	 (result (dfa-less superpos not-overlapped)))
    (dfa->nfa result)))

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
  (let ((fluentstr (read-line)) 
	(l1 (read-line))
	(l2 (read-line)))
    (print "e1:" l1 " e2:" l2)
    (let* ((fluents (first (str->snapshot-seq fluentstr)))
	   (e1 (regex->nfa l1))
	   (e2 (regex->nfa l2))
	   (alphabet (powerset fluents)))
      (write (nfa-alphabet e1))
      (write alphabet)
      (nfa-alphabet-set! e1 alphabet)
      (nfa-alphabet-set! e2 alphabet)
      (let loop ()
	  (let ((l (read-line)))
	    (if (not (equal? l #eof-object))
		(let ((str (str->snapshot-seq l)))
		  ;; for each allen relation build the dfa and run it on the input
		  ;; and print the results
 		  (print "---------------------------------")
 		  (print str)
;; 		  (print-nfa e1)
;; 		  (print-nfa e2)
;; 		  (print alphabet)
;; 		  (print-dfa (nfa->dfa e1))
;; 		  (print "1---")
;; 		  (print-dfa (equal e1 e2))
;; 		  (print "----")
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
