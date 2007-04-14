(module 
 eventlogic
 (main eventlogic-main)
 (include "nfa.sch")
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
         (graph "graph.scm")
	 (allen-nfa "allen-nfa.scm")
;	 (transducer "transducer.scm")
         (snapshots "snapshots.scm")))

(define *default-and-relations* 
  '(equal))
(define *default-tense-relations* 
  '(equal overlaps overlapped-by starts started-by ends ended-by during contains))
(define *primitive-events* '(A B C D begin_i end_i))
(define *alphabet* 
  (powerset *primitive-events*))

(define (ev-primitive p)
  ;; Primitive event types are liquid/homogenous
  ;; [p]+
  (if (not (member p *primitive-events*))
      (error "ev-primitive" "is not a primitive event type" p))
  (let ((p-nfa (nfa-plus (nfa-for-one-symbol (list p)))))
    (nfa-alphabet-set! p-nfa *alphabet*)
    p-nfa))
	   

(define (ev-and ev1 ev2 relations)
  ;; (and A B relations)
  (let loop ((nfa1 (allen (car relations) ev1 ev2))
	     (relations (cdr relations)))
    (if (not (null? relations))
	(loop (nfa-or nfa1
		      (allen (car relations) ev1 ev2))
	      (cdr relations))
	nfa1)))

(define (ev-or ev1 ev2)
  ;; (or A B)
  (nfa-or ev1 ev2))

(define (tense-interval)
  (let* ((interval (symbol->string (gensym "i")))
	 (beg (string->symbol
	       (string-append "begin_" interval)))
	 (end (string->symbol
	       (string-append "end_"  interval))))
    (nfa-concat
     (nfa-for-one-symbol (list beg))
     (nfa-star (nfa-empty))
     (nfa-for-one-symbol (list end)))))

(define (ev-tense ev1 relations)
  (let* ((interval (tense-interval))
	 (nfa1 (allen (car relations) ev1 (tense-interval)))
	 (new-alphabet (union-fluent-alphabets (nfa-alphabet nfa1) 
					       (nfa-alphabet interval))))
    (nfa-alphabet-set! nfa1 new-alphabet)
    (print-nfa nfa1)
    (print "---------------------")
    (if (not (null? (cdr relations)))
	(ev-tense nfa1 (cdr relations))
	nfa1)))

(define (dfa-consistent-universal-language)
  ;; TODO
  (dfa-universal *alphabet*))
	 
(define (ev-not ev1)
  (let ((dfa-not-ev1 (dfa-less (nfa->dfa ev1) (dfa-consistent-universal-language))))
    (dfa->nfa dfa-not-ev1)))

(define (event-formula->nfa formula)
  (match-case formula
	      ((not_ ?A)
	       (ev-not (event-formula->nfa A)))
	      ((or_ ?A ?B)
	       (ev-or (event-formula->nfa A)
		      (event-formula->nfa B)))
	      ((and_ ?A ?B)
	       (ev-and (event-formula->nfa A) 
		       (event-formula->nfa B)
		       *default-and-relations*))
	      ((and_ ?A ?B ?R)
	       (ev-and (event-formula->nfa A)
		       (event-formula->nfa B)
		       R))
	      ((tense ?A)
	       (ev-tense (event-formula->nfa A)
			 *default-tense-relations*))
	      ((tense ?A ?R)
	       (ev-tense (event-formula->nfa A)
			 R))
	      (?primitive
	       (ev-primitive primitive))))

(define (scene-desc->list scene-desc)
  '(scene-list))


(define (eventlogic-main args)
  (let loop ()
    (display "> ")
    (let* ((nfa1 (event-formula->nfa (read)))
	  (dfa1 (nfa->dfa nfa1)))
      (print-nfa nfa1)
      (print "*************************")
      (print (maxf length (nfa-alphabet nfa1)))
      (map print (nfa-alphabet nfa1))
      (view (graph dfa1))
      (loop))))
    
	    
