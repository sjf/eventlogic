(module 
 eventlogic
 (main event-logic-main)
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
         (graph "graph.scm")
	 (allen-nfa "allen-nfa.scm")
	 (transducer "transducer.scm")
         (snapshots "snapshots.scm")))

(define *default-and-relations* (list equals))
(define *default-tense-relations 
  (list equals overlaps overlapped-by starts started-by finishes finished-by during contains))
(define *primitive-events* '(A B C E F G H))
(define *alphabet* (powerset *primitive-events*))

(define (ev-primitive p)
  ;; [p]+
  (let ((p-nfa (nfa-plus (nfa-for-one-symbol p))))
    (nfa-alphabet-set! *alphabet)))
	   

(define (ev-and ev1 ev2 relations)
  ;; (and A B relations)
  (let loop ((nfa1 ((car relations) ev1 ev2))
	     (relations (cdr relations)))
    (if (not (null? relations))
	(loop (nfa-or nfa1
		      ((car relations) ev1 ev2))
	      (cdr relations))
	nfa1)))

(define (ev-or ev1 ev2)
  ;; (or A B)
  (nfa-or ev1 ev2))

(define (tense-interval)
  (nfa-concat
   (nfa-for-one-symbol 'begin_i)
   (nfa-empty-plus)
   (nfa-for-one-symbol 'end_i)))

(define (ev-tense ev1 relations)
  (let loop ((nfa1 ((car relations) ev1 (tense-interval)))
	     (relations (cdr relations)))
    (if (not (null? relations))
	(loop (nfa-or nfa1 
		      ((car relations) ev1 (tense-interval)))
	      nfa1))))
(define (dfa-consistent-universal-language)
  ;; TODO
  (dfa-universal *alphabet*))
	 
(define (ev-not ev1)
  (let ((dfa-not-ev1 (dfa-less (nfa->dfa ev1) (non-c-universal-language))))
    (dfa->nfa dfa-not-ev1)))

(define (event-formula->fsm formula)
  (match-case formula
	      ((not ?A)
	       (ev-not (event-formula->fsm A)))
	      ((or ?A)
	       (ev-or (event-forumla->fsm A)))
	      ((and ?A ?B)
	       (ev-and (event-formula->fsm A) 
		       (event-formula->fsm B)
		       *default-and-relations*))
	      ((and ?A ?B ?R)
	       (ev-and (event-formula->fsm A)
		       (event-formula->fsm B)
		       R))
	      ((tense ?A)
	       (ev-tense (event-formula->fsm A)
			 *default-tense-relations*))
	      ((tense ?A ?R)
	       (ev-tense (event-formula->fsm A)
			 R))
	      (?primitve
	       (ev-primitive primitive))))

(define (scene-desc->list scene-desc)
  '(scene-list))

