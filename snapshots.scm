;;rename this file to fst
(module 
 snapshots
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
	 (graph "graph.scm"))
; (main main-snapshots)
 (export (str->snapshot-seq str)
	 empty-snapshot
	 (nfa-empty-plus)
	 (nfa-empty)
	 (union-fluent-alphabets a1 a2)
	 (sort-fluents l)
	 (superposition a b)
	 (subsumptive-closure a)
	 (weak-subsumptive-closure a)))


(define snapshot-lexer
  (regular-grammar ()
   ((+ (or #\tab #\space)) (ignore))
   ((+ (or alpha digit (in "_-"))) (cons 'CHARS (the-string)))
   ("[" 'OPENB)
   ("]" 'CLOSEB)))

(set! *debug-parser* #f)
(define snapshot-grammar
  (lalr-grammar
   ; terminals
   (OPENB CLOSEB CHARS)
   ; productions
   (start ((snapshotlist)     
	   snapshotlist))
   (snapshotlist 
    ((snapshot snapshotlist)
     (cons snapshot snapshotlist))
    (() '()))
   (snapshot ((OPENB CLOSEB) 
	       '())
	      ((OPENB CHARS charslist CLOSEB)
	       (cons CHARS charslist)))
   (charslist ((CHARS charslist)
	       (cons CHARS charslist))
	      (()
	       '()))))

(define (str->snapshot-seq str)
  (read/lalrp
   snapshot-grammar
   snapshot-lexer
   (open-input-string str)))

;; Return a new transition where the input symbol is the union
;; of the symbols of trans1 trans2
;; The new current and next state will be pairs of the corresponding
;; states from trans1 and trans2.
(define (superpos-transition trans1 trans2)
  (let ((qc1 (first trans1)) ;; current state
	(qc2 (first trans2))
	(sym1 (second trans1)) ;; input symbol, must be a list
	(sym2 (second trans2))
	(qn1 (third trans1)) ;; next state
	(qn2 (third trans2)))
    (list (list qc1 qc2) (union-snapshot sym1 sym2) (list qn1 qn2))))

(define (union-snapshot snapshot1 snapshot2)
  (sort-fluents (union snapshot1 snapshot2)))
					

;; Return the dfa that is the superposition of dfa1 and dfa2
(define (superposition dfa1 dfa2)
  (let* ((new-start (list (dfa-start-state dfa1) (dfa-start-state dfa2)))
	 (new-final (cross-product (dfa-final-states dfa1) (dfa-final-states dfa2)))
	 (trans1 (dfa-transition-list dfa1))
	 (trans2 (dfa-transition-list dfa2))
	 (new-trans (nub 
		     (map (lambda (pair) (apply superpos-transition pair))
			  (cross-product trans1 trans2))))
	 ;; TODO: this isn't really correct
	 ;; it should be powerset(phi(dfa1) union phi(dfa2))
	 (new-alphabet (dfa-alphabet dfa1))
	 (dfa-new (dfa-rename-states 
		   (dfa new-start
			new-trans
			new-final
			new-alphabet))))
    (dfa-remove-unreachable-states! dfa1)
    ;(dfa-minimize! dfa-new)    
    dfa-new))
;     (map print trans1)
;     (map print trans2)
;     (map print new-trans)


;; The subsumptive closure, L |>=, is
;; the same as L&E*
(define (subsumptive-closure dfaA)
  (superposition dfaA (dfa-universal (dfa-alphabet dfaA))))

(define (weak-subsumptive-closure dfaA)
  ;; The strings which subsume L but can be of any length
  ;; []*.L.[]* & E*
  (let* ((nfa1 (dfa->nfa dfaA))
	 (weak-L (nfa-concat (nfa-star (nfa-empty)) nfa1 (nfa-star (nfa-empty))))
	 (weak-dfa (nfa->dfa weak-L)))
    (subsumptive-closure weak-dfa)))

(define (constraint dfaA dfaB)
  ;; A and B need to have the same alphabet
  (let* ((alphabet (dfa-alphabet dfaA))
	 (s-closure-A (subsumptive-closure dfaA))
	 (s-closure-B (subsumptive-closure dfaB))
	 (s-closure-B-inv (dfa-complement s-closure-B))
	 (intersec (dfa-intersection s-closure-A s-closure-B-inv))
	 (temp 
	  (dfa-concat (dfa-universal alphabet)
		      intersec
		      (dfa-universal alphabet)))
	 (result (dfa-complement temp)))
    ;(print "****")
    ;(print-dfa temp)
    ;(print-dfa result)
    ;(print "****")
    ;(view (graph temp))
    ;(view (graph result))
    result))	 
 ;   (map (lambda (d) (write "*") (read) (view (graph d)))
;	 (list s-closure-A s-closure-B s-closure-B-inv intersec temp))


;; the empty snapshot: []
(define empty-snapshot (list))

(define (nfa-empty)
  (nfa-for-one-symbol empty-snapshot))

;; an nfa which accepts []+
(define (nfa-empty-plus)
  (nfa-plus (nfa-empty)))

;; add a fluent to the set of fluents that
;; make up the alphabet of a dfa.
;; The fluents should really be stored in the dfas
(define (union-fluent-alphabets alphabet1 alphabet2)
  (let* ((fluents1 (nub (flatten alphabet1)))
	 (fluents2 (nub (flatten alphabet2)))
	 (new-alphabet (powerset (union fluents1 fluents2))))
    new-alphabet))
       
(define (sort-fluents l)
  (sort l
	(lambda (a b)
	  (string<?
	   (if (symbol? a) (symbol->string a) a)
	   (if (symbol? b) (symbol->string b) b)))))

(define A
  (dfa 
   'q0
   '((q0 (a) q1))
   '(q1)
   '(() (a) (b) (a b))))
(define B
  (dfa
   'qA
   '((qA (b) qB))
   '(qB)
   '(() (a) (b) (a b))))


(define (main-snapshots argv)
  (let ((c (constraint A B)))
    '()))
    ;(map print-dfa (list A B c))))
;    (view (graph c))))
;;   (if (> (length argv) 1)
;;       (begin
;; 	(print (str->snapshot-seq (string-join (cdr argv) " ")))
;; 	(print "A")
;; 	(print (str->snapshot-seq "[a][a b][a b][a]"))
;; 	(print "B")
;; 	(print-dfa (superposition test-dfa test-dfa1)))))
 
