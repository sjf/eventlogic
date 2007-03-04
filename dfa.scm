(module dfa
  (include "dfa.sch")
  (import (utils "utils.scm")
	  (graph "graph.scm"))
  ;(main main)
  (export
    (print-dfa x)
    (run-dfa x input)
    (make-transition-function l . cmp)
    (inverse dfaA)
    (complete dfaA)
    (intersection dfaA dfaB)
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
		(next-state ((dfa-transition dfa) curr-state symbol)))
					;(print (list "next: " next-state))
	   (cond
	    ;; this happens if the machine reached the end of input
	    ;; without accepting or encountered a symbol not in
	    ;; the alphabet
	    ((equal? next-state #f)
	     (list #f input))
	    (else
	     (%run-dfa next-state dfa (cdr input))))))))



;; transitions should be the transition table for a dfa or nfa,
;; a list of three element lists containing:
;; (current_state input_symbol next_state(s))
;; Returns a function which looks at this table to get
;; the next state. (For dfa next_state will be a symbol, for
;; a nfa it will be a list of states).
(define (make-transition-function transitions . cmp )
  ;; Look for a list list (curr-state symbol _ ) in transitions
  ;; the third element of this list will be the next state(s)
  ;; if no matching combination of curr-state and symbol are found
  ;; #f is returned
  (let ((cmp (if (null? cmp)
		 equal?
		 cmp)))
  (lambda (curr-state symbol)
    (let ((trans (find-if (lambda (t)
			    (and (equal? (first t) curr-state)
				 (cmp (second t) symbol)))
			  transitions)))
      (if (eq? trans #f)
	  #f
	  (third trans))))))


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
		    (if (equal? ((dfa-transition dfaA) (car states) (car alphabet)) #f)
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
	   (dfa (dfa-start-state dfaA)
		(make-transition-function transitions)
		transitions
		(dfa-final-states dfaA)
		(dfa-alphabet dfaA))))))
	

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
	  (dfa (dfa-start-state cdfa)
	       (dfa-transition cdfa)
	       (dfa-transition-list cdfa)
	       new-final
	       (dfa-alphabet cdfa))))))

(define (intersection dfaA dfaB)
  #f)
      
(define test-dfa 
  (dfa
   'q0
   (make-transition-function (list (list 'q0 'a 'q1)
				   (list 'q1 'b 'q0)
				   (list 'q0 'c 'q2)))
   (list (list 'q0 '(a) 'q1)
	 (list 'q1 '(b) 'q0)
	 (list 'q0 '(c) 'q2))
   (list 'q2)
   '(a b c)))

(define test-dfa1 
  (dfa
   'qA
   (make-transition-function (list (list 'qA '(d) 'qB)
				   (list 'qB '(e) 'qA)
				   (list 'qA '(f) 'qB)))
   (list (list 'qA '(d) 'qB)
	 (list 'qB '(e) 'qA)
	 (list 'qA '(f) 'qB))
   (list 'qB)
   '((d) (e) (f))))

(define (main argv)
; some tests
(if #t
(begin
(print-dfa test-dfa)
(print "should accept")
(print (run-dfa test-dfa '( a b c d e)))
(print "should reject")
(print (run-dfa test-dfa '( a b d e)))
(print-dfa (complete test-dfa))
(print-dfa (inverse test-dfa))
(print-dfa (complete test-dfa1))
(print-dfa (inverse test-dfa1)))))

;;
