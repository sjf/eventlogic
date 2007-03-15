(module 
 nfa
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm") 
	 (utils "utils.scm"))
 ;(main main-nfa)
 (export
  (print-nfa x)
  (make-transition-function-nfa l . cmp)
  (nfa->dfa nfa)
  (nfa-rename-states nfa1)
  (nfa-trans nfa state sym)
  test-nfa1
  test-nfa2
  test-nfa3))

(define (nfa-trans nfa curr-state symbol)
  (let ((states
	 (find-if-all
	  (lambda (t)
	    (and (equal? (first t) curr-state)
		 (equal? (second t) symbol)))
	  (nfa-transition-list nfa))))
    (if (null? states) 
	#f
	(map third states))))

(define (print-nfa x)
(let ((out (current-output-port)))
  (fprintf  out "nfa 
     alphabet: ~a
     states: ~a
     start: ~a
     transitions:~%"
     (nfa-alphabet x)
     (nfa-states x)
     (nfa-start-state x))
  (map (lambda (t) (fprintf out "         ~a~%" t)) (nfa-transition-list x))
  (fprintf out "     final-states: ~a~%"
     (nfa-final-states x))))



;; The nfa version differs from the dfa transition function
;; in that it will return a list
;; of next states instead of just one state		

;; transitions should be a list where each element is of the form
;; (current-state input-symbol next-state)
;; There may be more than one 

;; cmp is an optional function that will be used to compare the input
;; symbols for equality
(define (make-transition-function-nfa transitions . cmp)
  (let ((cmp (if (null? cmp)
		 equal?
		 cmp)))
  (lambda (curr-state symbol)
    (let ((trans (find-if-all (lambda (t)
				(and (equal? (first t) curr-state)
				     (cmp (second t) symbol)))
			      transitions)))
      (if (eq? trans (list))
	  #f
	  (map caddr trans)))))) ;; return only the next states

;; Generate new symbols for the states in a dfa
;; This is used after converting an nfa to dfa
;; to get rid of the long compound state identifiers
;; and give them more human friendly names.
;; Or if you are doing an operation on a dfa
;; which produces a new dfa, it needs to have
;; different state names to old one.
(define (dfa-rename-states dfa1)
  (let* ((mapping (make-hashtable))
	 (old-states (dfa-states dfa1)))
    ;; generate new symbols for each state
    (map (lambda (state)
	   (hashtable-put! mapping (to-string state) (gensym "q")))
	 old-states)
    (let ((new-trans
	   (map (lambda (trans)
		  (list 
		   (hashtable-get mapping (to-string (first trans)))
		   (second trans)
		   (hashtable-get mapping (to-string (third trans)))))
		(dfa-transition-list dfa1)))
	  (new-start (hashtable-get mapping (to-string (dfa-start-state dfa1))))
	  (new-final (map (lambda (state)
			    (hashtable-get mapping (to-string state)))
			  (dfa-final-states dfa1))))
      (dfa 
       new-start
       (make-transition-function new-trans)
       new-trans
       new-final
       (dfa-alphabet dfa1)))))	 

;; Acts the same as dfa-rename-states
(define (nfa-rename-states nfa1)
  (let* ((mapping (make-hashtable))
	 (old-states (nfa-states nfa1)))
    ;; generate new symbols for each state
    (map (lambda (state)
	   (hashtable-put! mapping (to-string state) (gensym "q")))
	 old-states)
    (let ((new-trans
	   (map (lambda (trans)
		  (list 
		   (hashtable-get mapping (to-string (first trans)))
		   (second trans)
		   (hashtable-get mapping (to-string (third trans)))))
		(nfa-transition-list nfa1)))
	  (new-start (hashtable-get mapping (to-string (nfa-start-state nfa1))))
	  (new-final (map (lambda (state)
			    (hashtable-get mapping (to-string state)))
			  (nfa-final-states nfa1))))
      (nfa 
       (nfa-alphabet nfa1)
       (hashtable->list mapping) ;; list of new states
       new-start
       (make-transition-function-nfa new-trans)
       new-trans
       new-final))))
  

;; Convert a non-deterministic epsilon automaton to a deterministic
;; epsilon-free automaton
(define (nfa->dfa nfa)
  (let* ((dfa-start-state (e-closure nfa (list (nfa-start-state nfa))))
	(dfa-transitions-and-states (nfa->dfa-transitions nfa))
	(dfa-transitions (first dfa-transitions-and-states))
	(dfa-states (second dfa-transitions-and-states))
	(dfa-final-states (nfa->dfa-final-states 
			   (nfa-final-states nfa)
			   dfa-states))
	(dfa1 (dfa dfa-start-state
		  (make-transition-function (first dfa-transitions-and-states))
		  dfa-transitions
		  dfa-final-states
		  (remq 'epsilon (nfa-alphabet nfa)))) ;; the same alphabet minus epsilon
	(dfa2 (dfa-rename-states dfa1)))
    dfa2))

(define (nfa->dfa-final-states nfa-final-states dfa-states)
  ;; if any of the nfa-states in the dfa-state are final
  ;; then the dfa-state will be a final state too.
  (find-if-all (lambda (dfa-state)
		 (find-if (lambda (state)
			    (member state nfa-final-states))
			  dfa-state))
	       dfa-states))
  

(define (nfa->dfa-transitions nfa)
  ;; Each dfa-state is the set of nfa-states which can be reached by the machine
  ;; a certain input
  ;; Dfa-marked states are the states for which all the reachable states have
  ;; found. We just need to remember these to avoid adding duplicate transitions
  ;; dfa-transitions will be (set_of_nfa_states symbol set_of_nfa_states)

  (let loop ((dfa-states
	      ;; Start with the e-closure of the start state
	      (list (e-closure nfa (list (nfa-start-state nfa)))))
	     (dfa-states-marked (list)) ;; Start off empty
	     (dfa-transitions (list)))  ;; Start off empty
    ;; Loop over dfa-states
    (if (not (null? dfa-states))
	;; Each dfa state is a set of nfa states
	(let ((nfa-states (car dfa-states)))
	  ;; mark this dfa-state
	  (set! dfa-states-marked (cons (car dfa-states) dfa-states-marked))
	  (set! dfa-states (cdr dfa-states))
	  ;; for each symbol in the alphabet...
	  (map (lambda (symbol)
		 (if (not (equal? symbol 'epsilon))
		     ;; The epsilon closure of all the states reachable from
		     ;; the current one over the current symbol
		     (let ((new-dfa-state
			    (e-closure nfa
				       (nfa-move nfa nfa-states symbol))))
		       (if (not (null? new-dfa-state))
			   (begin
			     (if (not (or (member new-dfa-state dfa-states)
					  (member new-dfa-state dfa-states-marked)))
				 ;; This is a new state, so we will have to add it
				 ;; to the list of states we have to find transitions for
				 (set! dfa-states (cons new-dfa-state dfa-states)))
			     ;; rememeber this transition
			     (set! dfa-transitions (cons (list nfa-states symbol new-dfa-state)
							 dfa-transitions)))))))
	       (nfa-alphabet nfa))
	  ;; loop over dfa-states
	  (loop dfa-states
		dfa-states-marked
		dfa-transitions))
	;; return the transitions
	(list dfa-transitions dfa-states-marked))))


;; Return that states can be reached from the given states over a
;; certain symbol
(define (nfa-move nfa states symbol)
  ;(print "++nfa-move" states symbol)
  (let* ((state-lists (map (lambda (state) (nfa-trans nfa state symbol)) states))
	 (non-false-states (list-remove-all state-lists #f))
	 (flattened-state-list (reduce append (list) non-false-states))
	 (unique-states (nub flattened-state-list)))
    ;(print "++" unique-states)
    ;;     (print state-lists)
;;     (print non-false-states)
    ;;     (print flattened-state-list)
    unique-states))


;; Get the epsilon closure of a set of states
;; These are the states that are reachable only by
;; epsilon transitions.
(define (e-closure nfa states)
  ;(print "-- e-closure " states)
  ;; include the input states in the closure
  ;; since a state can always go to itself via epsilon

  ;; Sort the closure because while in theory it is a set
  ;; it is implemented as a list.
  (sort (%e-closure nfa states states)
	(lambda (a b) (string<?
		       (if (symbol? a) (symbol->string a) a)
		       (if (symbol? b) (symbol->string b) b)))))

	


(define (%e-closure nfa states closure)
  (if (null? states)
      closure
      ;; Pop the first state from states,
      ;; then get the states reachable from it over epsilon
      
      (let* ((state (car states))
	     (e-trans-states (nfa-trans nfa state 'epsilon))
;; 	     (dummy (begin (print e-trans-states)
;; 			   (map print (nfa-transition-list nfa))
;; 			   (print "**"  state)))
		    
			   
             ;make sure we always have a set, and not #f or list with duplicates
	     (valid-e-trans-states (cond ((equal? #f e-trans-states) (list))
					 (else (nub e-trans-states))))
	     ;the states that aren't already recorded as reachable
	     (new-states (list-less valid-e-trans-states closure))
	     ;; add the new states to the list of states to search
	     (states (append (cdr states) new-states))
	     ;; add the new states to the epsilon closure
	     (closure (append closure new-states)))
        (%e-closure nfa states closure))))


;; do some tests
(define test-nfa1 (nfa
		  (list 'a 'b 'epsilon)
		  (list 'q0 'q1 'q2 'q3)
		  'q0
		  (make-transition-function (list (list 'q0 'a       (list 'q1))
						  (list 'q0 'epsilon (list 'q2 'q3))
						  (list 'q1 'b       (list 'q3))
						  (list 'q2 'epsilon (list 'q3))
						  (list 'q3 'epsilon (list 'q3 'q4))))
		  (list (list 'q0 'a       (list 'q1))
			(list 'q0 'epsilon (list 'q2 'q3))
			(list 'q1 'b       (list 'q3))
			(list 'q2 'epsilon (list 'q3))
			(list 'q3 'epsilon (list 'q3 'q4)))
		  (list 'q3)))

(define test-nfa2 (nfa
		  (list 'a 'b 'c)
		  (list 'q0 'q1 'q2 'q3)
		  'q0
		  (make-transition-function (list (list 'q0 'a       (list 'q1))
						  (list 'q1 'b       (list 'q2))
						  (list 'q1 'a       (list 'q1 'q2))
						  (list 'q2 'c       (list 'q3))))
		  (list (list 'q0 'a       (list 'q1))
			(list 'q1 'b       (list 'q2))
			(list 'q1 'a       (list 'q1 'q2))
			(list 'q2 'c       (list 'q3)))
		  (list 'q3)))


;; (a|b)*abb
(define test-nfa3 (nfa
		  (list 'a 'b 'epsilon)
		  (list 'q0 'q1 'q2 'q3 'q4 'q5 'q6 'q7 'q8 'q9 'q10)
		  'q0
		  (make-transition-function (list (list 'q0 'epsilon (list 'q1 'q7))
						  (list 'q1 'epsilon (list 'q2 'q4))
						  (list 'q2 'a       (list 'q3))
						  (list 'q3 'epsilon (list 'q6))
						  (list 'q4 'b       (list 'q5))
						  (list 'q5 'epsilon (list 'q6))
						  (list 'q6 'epsilon (list 'q7 'q1))
						  (list 'q7 'a       (list 'q8))
						  (list 'q8 'b       (list 'q9))
						  (list 'q9 'b       (list 'q10))))
		  (list (list 'q0 'epsilon (list 'q1 'q7))
			(list 'q1 'epsilon (list 'q2 'q4))
			(list 'q2 'a       (list 'q3))
			(list 'q3 'epsilon (list 'q6))
			(list 'q4 'b       (list 'q5))
			(list 'q5 'epsilon (list 'q6))
			(list 'q6 'epsilon (list 'q7 'q1))
			(list 'q7 'a       (list 'q8))
			(list 'q8 'b       (list 'q9))
			(list 'q9 'b       (list 'q10)))
		  (list 'q10)))

(define (main-nfa argv)
(if #t
    (begin
;(print "bleh")
;(print-nfa test-nfa3 (current-output-port))
(print-dfa (nfa->dfa test-nfa1))
(print-dfa (nfa->dfa test-nfa2))
(print-dfa (nfa->dfa test-nfa3))

;(print ((nfa-transition test-nfa) 'q0 'epsilon))

;(print (run-dfa test-dfa (list 'a 'b 'a 'b 'c)))

;(print (e-closure test-nfa (list (nfa-start-state test-nfa))))

;(print (nfa-move test-nfa2 (list 'q0 'q1 'q3) 'a))

;(define dfa1 (nfa->dfa test-nfa3))

;(nfa->dfa-final-states (list 1 2 3 4) (list (list 1 2 4 5)
;						     (list 5 6 7)
;						     (list 1)
;						     (list 2 3)
;						     (list 6 7 8 4))))

(define dfa (nfa->dfa test-nfa3))
(print "should accept")
(print (run-dfa dfa (list 'a 'b 'a 'b 'b)))
(print "should accept")
(print (run-dfa dfa (list 'a 'b 'b 'b)))
(print "should reject")
(print (run-dfa dfa (list 'a 'b 'a 'b )))
)))
;; Andrei loves you :)
;; He wants you to stay
;; Will miss you a lot..
;; Is quite sad that you're leaving
;; Love you too 
(define (love) #t) 

