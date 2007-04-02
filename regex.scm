(module 
 regex
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm"))
; (main main-regex)
 (export (nfa-plus nfa1)
	 (nfa-concat nfaA nfaB . rest)
	 (nfa-star nfa1)
	 (nfa-or nfaA nfaB)
	 (nfa-for-one-symbol sym)
	 (regex->dfa str)
	 (regex->nfa str)
	 (str->parse-tree str)
	 (parse-tree->nfa tree)))

(define regex-lexer
  (regular-grammar ()
   ((+ (or #\tab #\space #\newline)) (ignore))
   ((+ (or alpha digit (in "_-"))) (cons 'CHARS (the-string)))
   ("[" 'OPEN_SQBRACKET)
   ("]" 'CLOSE_SQBRACKET)
   ("(" 'OPENPARAN)
   (")" 'CLOSEPARAN)
   ("|" 'BAR)
   ("." 'DOT)
   ("*" 'STAR)
   ("+" 'PLUS)))

(define regex-grammar
  (lalr-grammar
   ; terminals  (PLUS isn't implemented in the grammar)
   (CHARS DOT STAR BAR OPENPARAN CLOSEPARAN OPEN_SQBRACKET CLOSE_SQBRACKET PLUS)
   ;productions
   (regex ((orr) orr))
   (orr ((concat orlist)
	 (if (equal? #f orlist)
	     concat
	     (list 'orr concat orlist))))
   (orlist ((BAR concat orlist) 
	    (if (equal? #f orlist)
		concat
		(list 'orr concat orlist)))
	   (() #f))
   (concat ((star concatlist) 
	    (if (equal? #f concatlist)
		star
		(list 'concat star concatlist))))
   ;; dot is concatenation character
   (concatlist ((star concatlist) 
		(if (equal? #f concatlist)
		    star
		    (list 'concat star concatlist)))
	       ((DOT star concatlist) 
		(if (equal? #f concatlist)
		    star
		    (list 'concat star concatlist)))
	       (() 
		#f))
   (star ((sym STAR)
	  (list 'star sym))
	 ((sym) 
	  sym))
   (sym ((OPEN_SQBRACKET snapshot CLOSE_SQBRACKET) 
	 snapshot)
        ((OPENPARAN regex CLOSEPARAN)
	 regex))
   (snapshot
    ((CHARS snapshot) 
     (cons CHARS snapshot))
    (()
     (list)))))
  
(define (parse-tree->nfa tree)
  (match-case tree
	      ;; accepts regexA*
	      ((star ?A)
	       (nfa-star (parse-tree->nfa A)))
	      ;; accepts regexA.regexB
	      ((concat ?A ?B)
	       (nfa-concat (parse-tree->nfa A) (parse-tree->nfa B)))
	      ;; accepts regexA or regexB
	      ((orr ?A ?B)
	       (nfa-or (parse-tree->nfa A) (parse-tree->nfa B)))
	      ;; accepts a symbol
	      (?sym (nfa-for-one-symbol sym))))

(define (nfa-concat nfaA nfaB . rest)
  (print nfaA nfaB rest)
  (print-nfa (car rest))
  (let loop ((result (%nfa-concat nfaA nfaB))
	     (rest rest))
    (cond ((null? rest) result)
	  (else (loop 
		 (%nfa-concat result (car rest))
		 (cdr rest))))))
		 
(define (%nfa-concat nfaA nfaB)
  ;; build an nfa which accepts La.Lb, where La are strings accepted
  ;; by nfaA and Lb are strings accepted by nfaB
  (let* ((startA (nfa-start-state nfaA))
	 (startB (nfa-start-state nfaB))
	 (endB (nfa-final-states nfaB))
	 ;; Add epsilon transitions from each of the final 
	 ;; states of nfaA to the start state of nfaB
	 (trans  (append (map (lambda (endA)
				(list endA 'epsilon startB))
			      (nfa-final-states nfaA))
			 (nfa-transition-list nfaA) 
			 (nfa-transition-list nfaB))))
    (nfa-rename-states
     (nfa 
      (union (nfa-alphabet nfaA) (nfa-alphabet nfaB))
      (append (nfa-states nfaA) (nfa-states nfaB))
      startA
      trans
      endB))))

(define (nfa-star nfaA)
  ;; build an nfa which accepts L*
  ;; where L are strings accepted by the nfa
  (let* ((q0     (gensym "q"))
	 (qfinal (gensym "q"))
	 (startA (nfa-start-state nfaA))
	 ;; this only works for nfa which 
	 ;; only have one final state,
	 ;; which includes all the ones 
	 ;; produced from a regex
	 (endA   (car (nfa-final-states nfaA)))
	 (trans (append (list (list q0 'epsilon qfinal)
			      (list q0 'epsilon startA)
			      (list endA 'epsilon startA)
			      (list endA 'epsilon qfinal))
			(nfa-transition-list nfaA))))
    (nfa-rename-states
     (nfa
      (nfa-alphabet nfaA)
      (append (list q0 qfinal) (nfa-states nfaA))
      q0
      trans
      (list qfinal)))))

(define (nfa-or nfaA nfaB)
  ;; build an nfa which accepts La|Lb where La are strings accepted nfaA
  ;; and Lb are strings accepted by nfaB
  (let* ((q0    (gensym "q"))
	 (qfinal (gensym "q"))
	 (startA (nfa-start-state nfaA))
	 (startB (nfa-start-state nfaB))
	 (endA (car (nfa-final-states nfaA))) 
	 (endB (car (nfa-final-states nfaB)))
	 ;; This is only works for nfas which have one final
	 ;; state, which includes all the ones produced from a regex
	 (trans (append 
		 (list (list q0 'epsilon startA)
		       (list q0 'epsilon startB)
		       (list endA 'epsilon qfinal)
		       (list endB 'epsilon qfinal))
		 (nfa-transition-list nfaA) (nfa-transition-list nfaB))))
    (nfa-rename-states
     (nfa (union  (nfa-alphabet nfaA) (nfa-alphabet nfaB))
	  (append (nfa-states nfaA) (nfa-states nfaB) (list q0 qfinal))
	  q0
	  trans
	  (list qfinal)))))

(define (nfa-for-one-symbol sym)
  ;; build an nfa which accepts one symbol
  (let* ((q0 (gensym "q"))
	(qfinal (gensym "q"))
	(trans  (list (list q0 sym qfinal))))
    (nfa (list sym)
	      (list q0 qfinal)
	      q0
	      trans
	      (list qfinal))))

(define (nfa-plus nfa)
  ;; builds an nfa which accepts L+ where L are the strings accepted by 
  ;; the nfa
  (nfa-concat nfa (nfa-star nfa)))

(define (str->parse-tree str)
  (read/lalrp regex-grammar
	      regex-lexer
	      (open-input-string str)))  

;; returns an nfa
(define (regex->nfa str)
  (parse-tree->nfa 
   (str->parse-tree str)))

;; returns a dfa
(define (regex->dfa str)
  (nfa->dfa
   (regex->nfa str)))

(define (main-regex argv)
  (let ((r #f))
    (print
     (read/lalrp regex-grammar
		 regex-lexer
		 (open-input-string (string-join (cdr argv) " ")))))
  (print "blah"))

;; end
