(module 
 snapshots
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm"))
; (main main-snapshots)
 (export (str->snapshot-seq str)
	 empty-snapshot
	 (nfa-empty-plus)
	 (superposition a b)))


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
(define (union-transition trans1 trans2)
  (let ((qc1 (first trans1)) ;; current state
	(qc2 (first trans2))
	(sym1 (second trans1)) ;; input symbol, must be a list
	(sym2 (second trans2))
	(qn1 (third trans1)) ;; next state
	(qn2 (third trans2)))
    (list (list qc1 qc2) (union-snapshot sym1 sym2) (list qn1 qn2))))

(define (union-snapshot snapshot1 snapshot2)
  (sort (union snapshot1 snapshot2) (lambda (a b)
					(string<?
					 (if (symbol? a) (symbol->string a) a)
					 (if (symbol? b) (symbol->string b) b)))))
					

;; Return the dfa that is the superposition of dfa1 and dfa2
(define (superposition dfa1 dfa2)
  (let* ((start-state (list (dfa-start-state dfa1) (dfa-start-state dfa2)))
	 (final-states (cross-product (dfa-final-states dfa1) (dfa-final-states dfa2)))
	 (trans1 (dfa-transition-list dfa1))
	 (trans2 (dfa-transition-list dfa2))
	 (trans-new (map (lambda (pair) (apply union-transition pair))
			 (cross-product trans1 trans2)))
	 (alpha-new (map second trans-new)))
    (dfa-rename-states
     (dfa
      start-state
      trans-new
      final-states
      alpha-new))))

;; the empty snapshot: []
(define empty-snapshot (list))

;; an nfa which accepts []+
(define (nfa-empty-plus)
  (nfa-plus (nfa-for-one-symbol empty-snapshot)))


(define (main-snapshots argv)
  (if (> (length argv) 1)
      (begin
	(print (str->snapshot-seq (string-join (cdr argv) " ")))
	(print "A")
	(print (str->snapshot-seq "[a][a b][a b][a]"))
	(print "B")
	(print-dfa (superposition test-dfa test-dfa1)))))
 
