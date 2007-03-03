(module 
 regex
 (include "nfa.sch")
 (include "dfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm"))
 (export (regex->dfa str))
 (main main-regex))

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
   ; terminals (PLUS isn't implemented yet)
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
   (sym ((OPEN_SQBRACKET situationlist CLOSE_SQBRACKET) 
	 situationlist)
;; 	((OPEN_SQBRACKET CLOSE_SQBRACKET)  
;; 	 (list))
        ((OPENPARAN regex CLOSEPARAN)
	 regex))
   (situationlist   
    ((CHARS situationlist) 
     (cons CHARS situationlist))
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

(define (nfa-concat nfaA nfaB)
  ;; build an nfa which accepts La.Lb, where La are strings accepted
  ;; by nfaA and Lb are strings accepted by nfaB
  (let* ((startA (nfa-start-state nfaA))
	 (startB (nfa-start-state nfaB))
	 (endA   (car (nfa-final-states nfaA)))
	 (endB   (car (nfa-final-states nfaB)))
	 (trans  (append (list (list endA 'epsilon startB))
			 (nfa-transition-list nfaA) (nfa-transition-list nfaB))))
    (nfa 
     (union (nfa-alphabet nfaA) (nfa-alphabet nfaB))
     (append (nfa-states nfaA) (nfa-states nfaB))
     startA
     (make-transition-function-nfa trans)
     trans
     (list endB))))

(define (nfa-star nfa)
  ;; build an nfa which accepts L*
  ;; where L are strings accepted by the nfa
  (let* ((q0     (gensym "q"))
	 (qfinal (gensym "q"))
	 (startA (nfa-start-state nfa))
	 ;; this only works for nfa which 
	 ;; only have one final state,
	 ;; which includes all the ones 
	 ;; produced from a regex
	 (endA   (car (nfa-final-states nfa)))
	 (trans (append (list (list q0 'epsilon qfinal)
			      (list q0 'epsilon startA)
			      (list endA 'epsilon startA)
			      (list endA 'epsilon qfinal))
			(nfa-transition-list nfa))))
    (nfa
     (nfa-alphabet nfa)
     (cons q0 (cons qfinal (nfa-states nfa)))
     q0
     (make-transition-function-nfa trans)
     trans
     (list qfinal))))

(define (nfa-or nfaA nfaB)
  ;; build an nfa which accepts La|Lb where La are strings accepted nfaA
  ;; and Lb are strings accepted by nfaB
  (let* ((q0    (gensym "q"))
	 (qfinal (gensym "q"))
	 (startA (nfa-start-state nfaA))
	 (startB (nfa-start-state nfaB))
	 (endA (car (nfa-final-states nfaA))) 
	 (endB (car (nfa-final-states nfaB)))
	 (trans (append 
		 (list (list q0 'epsilon startA)
		       (list q0 'epsilon startB)
		       (list endA 'epsilon qfinal)
		       (list endB 'epsilon qfinal))
		 (nfa-transition-list nfaA) (nfa-transition-list nfaB))))
    (nfa (union  (nfa-alphabet nfaA) (nfa-alphabet nfaB))
	      (append (nfa-states nfaA) (nfa-states nfaB) (list q0 qfinal))
	      q0
	      (make-transition-function-nfa trans)
	      trans
	      (list qfinal))))

(define (nfa-for-one-symbol sym)
  ;; build an nfa which accepts one symbol
  (let* ((q0     (gensym "q"))
	(qfinal (gensym "q"))
	(trans  (list (list q0 sym qfinal))))
    (nfa (list sym)
	      (list q0 qfinal)
	      q0
	      (make-transition-function-nfa trans)
	      trans
	      (list qfinal))))

(define (nfa-plus nfa)
  ;; builds an nfa which accepts L+ where L are the strings accepted by 
  ;; the nfa
  (nfa-concat nfa (nfa-star nfa)))


;; returns an nfa
(define (regex->nfa str)
  (parse-tree->nfa 
   (read/lalrp regex-grammar
	       regex-lexer
	       (open-input-string str))))

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