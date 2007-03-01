(module dfa
  (include "dfa.sch")
  (import (utils "utils.scm"))
  ;(main main)
  (export
    (print-dfa x out)
    (run-dfa x input)
    (make-transition-function l . cmp)
    test-dfa
    test-dfa1))
    

(define (print-dfa x out)
  (fprintf  out "dfa 
     start: ~a
     transitions:~%"
     (dfa-start-state x))
  (map (lambda (t) (fprintf out "         ~a~%" t)) (dfa-transition-list x))
  (fprintf out "    final-states: ~a~%"
     (dfa-final-states x)))

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

(define test-dfa 
  (dfa
   'q0
   (make-transition-function (list (list 'q0 'a 'q1)
				   (list 'q1 'b 'q0)
				   (list 'q0 'c 'q2)))
   (list (list 'q0 '(a) 'q1)
	 (list 'q1 '(b) 'q0)
	 (list 'q0 '(c) 'q2))
   (list 'q2)))

(define test-dfa1 
  (dfa
   'qA
   (make-transition-function (list (list 'q0 'a 'q1)
				   (list 'q1 'b 'q0)
				   (list 'q0 'c 'q2)))
   (list (list 'qA '(d) 'qB)
	 (list 'qB '(e) 'qA)
	 (list 'qA '(f) 'qB))
   (list 'qB)))

(define (main argv)
; some tests
(if #t
(begin
(print-dfa test-dfa (current-output-port))
(print "should accept")
(print (run-dfa test-dfa '( a b c d e)))
(print "should reject")
(print (run-dfa test-dfa '( a b d e))))))
;;
