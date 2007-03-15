(module dfa
  (include "dfa.sch")
  (import (utils "utils.scm")
	  (graph "graph.scm"))
;  (main main)
  (export
    (print-dfa x)
    (run-dfa x input)
    (inverse dfaA)
    (complete dfaA)
    (dfa-intersection dfaA dfaB)
    (dfa-states dfa)
    (dfa-rename-states dfa)
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


;; A complete dfa has transitions out of every state for every symbol
;; in the language.
;; This will add in the necessary extra transitions, which will go
;; to a new sink state
(define (complete dfaA)
  (define transitions (dfa-transition-list dfaA))
  (define sink-state (gensym "sink"))
  (let loop0 ((states (nub (append 
			    (map first (dfa-transition-list dfaA))
			    (map third (dfa-transition-list dfaA))))))
;    (print states)
    (cond ((not (null? states))
	   (let loop1 ((alphabet (dfa-alphabet dfaA)))
	     ;(print alphabet)
	     (cond ((not (null? alphabet))
		   ; (print (car states) " " (car alphabet) "  "
			;   ((dfa-transition dfaA) (car states) (car alphabet)))
		    (if (equal? (dfa-trans dfaA (car states) (car alphabet)) #f)
			; there is no transition for this state and symbol
			; add a transition to the sink state
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
	

(define (inverse dfaA)
  (let* ((cdfa (complete dfaA))
	 (states (nub (append
		       (map first (dfa-transition-list cdfa))
		       (map third (dfa-transition-list cdfa))))))
    ; swap final and non final states
    (let loop ((states states)
	       (new-final (list)))
      (if (not (null? states))
	  (if (not (member (car states)
			   (dfa-final-states cdfa)))
	      (loop (cdr states) (cons (car states) new-final))
	      (loop (cdr states) new-final))
	  ; build a new dfa and return it
	  (dfa-rename-states
	   (dfa (dfa-start-state cdfa)
		(dfa-transition-list cdfa)
		new-final
		(dfa-alphabet cdfa)))))))

;; Returns a dfa that is the intersection of dfaA and dfaB
(define (dfa-intersection dfaA dfaB)
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

;; This is a hack because we don't bother storing the states
(define (dfa-states dfa1)
  (nub (append (map first (dfa-transition-list dfa1))
	       (map third (dfa-transition-list dfa1))
	       (dfa-final-states dfa1)
	       (list (dfa-start-state dfa1)))))
		


;; Generate new symbols for the states in a dfa
;; If you are constructing a dfa based on another
;; it must have different states names.
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
   (list (list 'q0 'a 'q1)
	 (list 'q1 'b 'q0)
	 (list 'q0 'c 'q2))
   (list 'q2)
   '(a b c)))
 
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
   (list 'qB)
   '((d) (e) (f))))

(define (main argv)
; some tests
(print-dfa test-dfa)
(print "should accept")
(print (run-dfa test-dfa '( a b c d e)))
(print "should reject")
(print (run-dfa test-dfa '( a b d e)))
(print-dfa (complete test-dfa))
(print-dfa (inverse test-dfa))
(print-dfa (complete test-dfa1))
(print-dfa (inverse test-dfa1))
;(show-graph (graph test-dfa))
(print-dfa (dfa-intersection (complete test-dfa) (complete test-dfaA)))
(print-dfa (dfa-intersection test-dfa test-dfaA))
;(show-graph (graph (dfa-intersection test-dfa test-dfa))))
)
;;
