(module dfa
  (include "dfa.sch")
  (include "nfa.sch")
  (import (utils "utils.scm")
	  (graph "graph.scm")
	  (nfa "nfa.scm")
	  (regex "regex.scm"))
;  (main main)
  (export
    (print-dfa dfaA)
    (run-dfa dfaA input)
    (dfa->nfa dfaA)
    (dfa-concat dfaA dfaB . rest)
    (dfa-universal alphabet)
    (dfa-complement dfaA)
    (dfa-complete dfaA)
    (dfa-intersection dfaA dfaB)
    (dfa-less dfaA dfaB)
    (dfa-states dfaA)
    (dfa-remove-unreachable-states! dfaA)
    (dfa-rename-states dfaA)
    test-dfa
    test-dfa1))
    
(define (print-dfa x)
  (let ((out (current-output-port)))
    (fprintf out "dfa 
     start: ~a
     transitions:~%"
	     (dfa-start-state x))
    (map (lambda (t) (fprintf out "         ~a~%" t)) (dfa-transition-list x))
    (fprintf out "    final-states: ~a~%"
	     (dfa-final-states x))))

;; Look for a list list (curr-state symbol _ ) in transitions
;; the third element of this list will be the next state(s)
;; if no matching combination of curr-state and symbol are found
;; #f is returned
(define (dfa-trans dfa curr-state symbol)
  (let ((trans (find-if (lambda (t)
			  (and (equal? (first t) curr-state)
			       (equal? (second t) symbol)))
			(dfa-transition-list dfa))))
    (if (eq? trans #f)
	#f
	;; return the next state
	(third trans))))
  

;; run a finite state machine on a certain input
;; returns two element list: #t of #f if the machine accepted or not
;; and the remaining input
(define (run-dfa dfa input)
  (%run-dfa (dfa-start-state dfa) dfa input))

(define (%run-dfa curr-state dfa input)
  ;(print curr-state " -> " (if (null? input) input (car input)))
  (cond ((member curr-state (dfa-final-states dfa))
	 (list #t input)) ;; accept
	((null? input)
	 (list #f '()))   ;; end of input and not accepting
	(else
	 (let* ((symbol (car input))
		(next-state (dfa-trans dfa curr-state symbol)))
					;(print (list "next: " next-state))
	   (cond
	    ;; this happens if the machine reached the end of input
	    ;; without accepting or encountered a symbol not in
	    ;; the alphabet
	    ((equal? next-state #f)
	     (list #f input))
	    (else
	     (%run-dfa next-state dfa (cdr input))))))))

(define (dfa->nfa dfaA)
  (nfa (dfa-alphabet dfaA)
       (dfa-states dfaA)
       (dfa-start-state dfaA)
       (dfa-transition-list dfaA)
       (dfa-final-states dfaA)))

;; Concatenate two dfas.
;; It uses the routine to concatenate nfas.
(define (dfa-concat dfaA dfaB . rest)
  (let* ((nfaC (apply nfa-concat (map dfa->nfa (cons dfaA (cons dfaB rest))))))
    (nfa->dfa nfaC)))

;; ;; Concatenate two dfas.
;; ;; The start state of B becomes the final state of dfaA.
;; ;; Replace each reference to the start state of dfaB
;; ;; with a references to the final states of dfaA.
;; (define (dfa-concat dfaA dfaB)
;;   (let* ((qfinal-A (dfa-final-states dfaA))
;; 	 (qfinal-B (dfa-final-states dfaB))
;; 	 (q0-B (dfa-start-state dfaB))
;; 	 (new-start (dfa-start-state dfaA))
;; 	 (new-final (if (member q0-B qfinal-B)
;; 			;; replace q0-B with the set qfinal-A
;; 			(append (remq q0-B qfinal-B)
;; 				qfinal-A)
;; 			qfinal-B))
;; 	 (new-trans-B (flatten 
;; 		       (map (lambda (qfinal)
;; 			      (map (lambda (trans)
;; 				     (let ((qA (if (equal? (first trans) q0-B)
;; 						   qfinal (first trans)))
;; 					   (qB (if (equal? (third trans) q0-B)
;; 						   qfinal (third trans))))
;; 				       (list qA (second trans) qB)))
;; 				   (dfa-transition-list dfaB)))
;; 			   qfinal-A)))
;; 	 (new-trans (append (dfa-transition-list dfaA) new-trans-B))
;; 	 (new-alphabet (union (dfa-alphabet dfaA) (dfa-alphabet dfaB))))
;;     (print new-trans)
;; ;  (dfa-rename-states 
;;    (dfa 
;;     new-start
;;     new-trans
;;     new-final
;;     new-alphabet)))

;; A complete dfa has transitions out of every state for every symbol
;; in the language.
;; This will add in the necessary extra transitions, which will go
;; to a new sink state
(define (dfa-complete dfaA)
  (define transitions (dfa-transition-list dfaA))
  (define sink-state (gensym "sink"))
  (let loop0 ((states (cons sink-state
			    (nub (append 
				  (map first (dfa-transition-list dfaA))
				  (map third (dfa-transition-list dfaA)))))))
;    (print states)
    (cond ((not (null? states))
	   (let loop1 ((alphabet (dfa-alphabet dfaA)))
	     ;(print alphabet)
	     (cond ((not (null? alphabet))
		   ; (print (car states) " " (car alphabet) "  "
			;   ((dfa-transition dfaA) (car states) (car alphabet)))
		    (if (equal? (dfa-trans dfaA (car states) (car alphabet)) #f)
			; There is no transition for this state and symbol
			; So add a transition to the sink state
			(set! transitions
			      (cons (list (car states)
					  (car alphabet)
					  sink-state)
				    transitions)))
		    (loop1 (cdr alphabet)))))
	   (loop0 (cdr states)))
	  (else
	   (dfa-rename-states 
	    (dfa (dfa-start-state dfaA)
		 transitions
		 (dfa-final-states dfaA)
		 (dfa-alphabet dfaA)))))))

(define (dfa-universal alphabet)
  (let* ((q0 (gensym "q"))
	 (trans (map 
		 (lambda (sym)
		   (list q0 sym q0))
		 alphabet)))
    (dfa 
     q0
     trans
     (list q0)
     alphabet)))
	 

(define (dfa-complement dfaA)
  (let* ((cdfa (dfa-complete dfaA))
	 (states (dfa-states cdfa))
	 ;; swap final and non final states
	 (new-final (list-less states (dfa-final-states cdfa))))
    ;; build the new dfa and return it
    (print "---")
    (print-dfa dfaA)
    (print-dfa cdfa)
    (print "---")
    (dfa-rename-states
     (dfa (dfa-start-state cdfa)
	  (dfa-transition-list cdfa)
	  new-final
	  (dfa-alphabet cdfa)))))

;; Returns a dfa that is the intersection of dfaA and dfaB
(define (dfa-intersection dfaA dfaB)
  (print-dfa dfaA)
  (print-dfa dfaB)
  (let* ((alphabet (union (dfa-alphabet dfaA)
			  (dfa-alphabet dfaB)))
	 (new-start (list (dfa-start-state dfaA)
			  (dfa-start-state dfaB))))
    (let loop ((new-states (list new-start))
	       (new-trans (list))
	       (states-to-search (list new-start)))
      (if (not (null? states-to-search))
	  (let* ((state (car states-to-search))
		 (curr-transitions (%intersecting-transitions 
				    dfaA dfaB 
				    (first state)
				    (second state)
				    alphabet))
		 (next-states (nub (map third curr-transitions))))
	    (loop (union next-states new-states)
		  (append new-trans curr-transitions)
		  (append (cdr states-to-search) (list-less next-states new-states))))
	  (dfa-rename-states
	   (dfa new-start
		new-trans
		(intersection 
		 (cross-product (dfa-final-states dfaA)
				(dfa-final-states dfaB))
		 new-states)
		alphabet))))))
  
;; return a set of merged transtions for dfaA and dfaB
;; from stateA and stateB over all the symbols in alphabet
(define (%intersecting-transitions dfaA dfaB stateA stateB alphabet)
  (let loop ((symbols alphabet)
	     (new-states (list))
	     (new-transitions (list)))
    (cond ((not (null? symbols))
	   (let* ((sym (car symbols))
		  (a-next (dfa-trans dfaA stateA sym))
		  (b-next (dfa-trans dfaB stateB sym)))
	     (if (not (or (eq? a-next #f) (eq? b-next #f)))
		 (let* ((state (list stateA stateB))
			(next-state (list a-next b-next))
			(new-trans (list state sym next-state)))
		   (loop (cdr symbols) 
			 (cons next-state new-states)
			 (cons new-trans new-transitions)))
		 (loop (cdr symbols)
		       new-states
		       new-transitions))))
	  (else 
	   new-transitions))))

(define (dfa-less dfa1 dfa2)
  (dfa-intersection dfa1 (dfa-complement dfa2)))
;;   (let* ((i (dfa-complement dfa2))
;; 	 (l (dfa-intersection dfa1 i)))
;;     (print "inv")(read)
;;     (show-graph (graph i))
;;     (print "dfa1")(read)
;;     (show-graph (graph dfa1))
;;     (print "intersection")(read)
;;     (show-graph (graph l))
;;     l))


(define (dfa-remove-unreachable-states! dfaA)  
  (let loop ((unreachable-states (%unreachable-states dfaA)))
    (cond ((not (null? unreachable-states))
	   ;; remove all the transitions that have
	   ;; an unreachable states as the source
	   (let ((new-trans (list-remove-if 
			     (lambda (trans)
			       (member (first trans) unreachable-states))
			     (dfa-transition-list dfaA))))
	     (dfa-transition-list-set! dfaA new-trans)
	     ;; remove any final states that are unreachable
	     (dfa-final-states-set! dfaA (list-less (dfa-final-states dfaA) unreachable-states))
	     (loop (%unreachable-states dfaA))))
	  (else	dfaA))))

;; Returns all the unreachable states in the dfa
(define (%unreachable-states dfaA)
  (let loop ((states (dfa-states dfaA))
	      (unreachable-states (list)))
    (if (not (null? states))
	(if (%unreachable-state? dfaA (car states))
	    (loop (cdr states) (cons (car states)
				     unreachable-states))
	    (loop (cdr states) unreachable-states))
	unreachable-states)))

(define (%unreachable-state? dfaA state)
  (not
   (find-if
    ;; states are reachable if..
    (lambda (transition)
      ;; the start state is always reachable
      (or (equal? state (dfa-start-state dfaA))
	  ;; this state is the destination of some transition
	  (and (equal? state (third transition))
	       ;; where it is not also the source 
	       (not (equal? state (first transition))))))
    (dfa-transition-list dfaA))))

;; This is a hack because we don't bother storing the states
(define (dfa-states dfa1)
  (nub (append (map first (dfa-transition-list dfa1))
	       (map third (dfa-transition-list dfa1))
	       (dfa-final-states dfa1)
	       (list (dfa-start-state dfa1)))))
		
;; Generate new symbols for the states in a dfa
;; If you are constructing a dfa based on another
;; it must have different symbols for the names of states.
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
       new-trans
       new-final
       (dfa-alphabet dfa1)))))	 
		    
  
(define test-dfa 
  (dfa
   'q0
   (list (list 'q0 '(a) 'q1)
	 (list 'q1 '(b) 'q0)
	 (list 'q0 '(c) 'q2))
   (list 'q2 'q0)
   '((a) (b) (c))))
 
(define test-dfaA
  (dfa
   'q0
   (list (list 'q0 'a 'q1)
	 (list 'q1 'b 'q0)
	 (list 'q0 'b 'q2))
   (list 'q2)
   '(a b c)))

(define test-dfa1 
  (dfa
   'qA
   (list (list 'qA '(d) 'qB)
	 (list 'qB '(e) 'qA)
	 (list 'qA '(f) 'qB))
   (list 'qB 'qA)
   '((d) (e) (f))))

(define test-dfa2
  (dfa
   'qA
   '((qA 1 qB)
     (qA 2 qC)
     (qA 3 qA)
     (qB 4 qE)
     (qD 3 qE)
     (qE 4 qE)
     (qE 5 qF))
   '(qA qE)
   '(1 2 3 4 5)))

(define A
  (dfa 
   'q0
   '((q0 a q1)
     (q1 b q0)
     (q0 c q2))
   '(q0)
   '(a b c)))
(define B
  (dfa
   'qA
   '((qA d qA))
   '(qA)
   '(d)))

(define (main argv)
  (print-dfa test-dfa)
  (print-dfa test-dfa1)
  (let ((c (dfa-concat test-dfa test-dfa1)))
    (print-dfa c)
    (show-graph (graph c)))
  (exit)

;  (show-graph (graph test-dfa2))
;  (show-graph (graph (dfa-remove-unreachable-states! test-dfa2)))
; some tests
(print-dfa test-dfa)
(print "should accept")
(print (run-dfa test-dfa '( a b c d e)))
(print "should reject")
(print (run-dfa test-dfa '( a b d e)))
(print-dfa (dfa-complete test-dfa))
(print-dfa (dfa-complement test-dfa))
(print-dfa (dfa-complete test-dfa1))
(print-dfa (dfa-complement test-dfa1))
;(show-graph (graph test-dfa))
(print-dfa (dfa-intersection (dfa-complete test-dfa) (dfa-complete test-dfaA)))
(print-dfa (dfa-intersection test-dfa test-dfaA))
;(show-graph (graph (dfa-intersection test-dfa test-dfa))))
(print "------")

	 
)
;;
