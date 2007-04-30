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

(define *primitive-events* '(A B C))
(define *inverses* '(notA notB notC))
(define *intervals* '())
(define *internals* '(and not or tense equal before after meets met-by overlaps operlapped-by contains during starts started-by ends ended-by weak-overlaps))

(define (add-interval-fluents! fluents)
  (set! *intervals* (append fluents *intervals*))
  (set! *alphabet* (build-alphabet)))

(define (build-alphabet)
  (let ((fluents (append *primitive-events*
			 *inverses*
			 *intervals*)))
  (map sort-fluents
       (powerset fluents))))

(define *alphabet* (build-alphabet))
(define (alphabet)
  *alphabet*)

(define (dfa-negation-constraints events inverses)
  ;; [phi,not_phi] => {}
  (let ((alpha (alphabet)))

    ;; not s1 and s2
    (define (inv-constraint sym sym-inv)
      ;(print alpha)
      (let ((dfa1 (dfa-for-one-symbol (list sym sym-inv) alpha))
	    (dfa2 (dfa-empty-language alpha)))
	(constraint dfa1 dfa2)))

    (let loop ((dfa1 (inv-constraint (car events)
				     (car inverses)))
	       (events (cdr events))
	       (inverses (cdr inverses)))
;;       (view (graph dfa1))
;;       (read)
       (dfa-remove-never-accepting-states! dfa1)
;;       (read)
;      (view (graph dfa1))
      (cond ((not (null? events))
	     (loop (dfa-intersection
		    dfa1
		    (inv-constraint
		    (car events)
		    (car inverses)))
		   (cdr events)
		   (cdr inverses)))
	    (else 
	     ;(view (graph dfa1))
	     dfa1)))))

(define (dfa-complete-constraints events inverses)
  ;; [] => [phi] or [not_phi]
  (define (comp-constraint sym sym-inv alpha)
    (let* ((dfa1 (dfa-for-one-symbol empty-snapshot alpha))
	   (d-sym (dfa-for-one-symbol (list sym) alpha))
	   (d-sym-inv (dfa-for-one-symbol (list sym-inv) alpha))
	   (nfa-or (nfa-or (dfa->nfa d-sym)
				     (dfa->nfa d-sym-inv)))
	   (dfa-or (nfa->dfa nfa-or)))
      (constraint dfa1 dfa-or)))
  (let ((alpha (alphabet)))
    (let loop ((dfa1 (comp-constraint (car events) (car inverses) alpha))
	       (events (cdr events))
	       (inverses (cdr inverses)))
      (dfa-remove-never-accepting-states! dfa1)
      (cond ((not (null? events))
	     (loop (dfa-intersection 
		    dfa1
		    (comp-constraint
		     (car events)
		     (car inverses)
		     alpha))
		   (cdr events)
		   (cdr inverses)))
	    (else
	     (dfa-really-minimize! dfa1)
	     dfa1)))))

(define (ev-subsumption nfa1)
  (let* ((dfa1 (nfa->dfa nfa1))
	(closure (subsumptive-closure dfa1))
	(intersec (dfa-intersection closure (dfa-consistent-universal-language))))
    (dfa->nfa intersec)))
	       
(define (ev-primitive p)
  ;; Primitive event types are liquid/homogenous
  ;; [p]+
  (if (not (member p *primitive-events*))
      (error "ev-primitive" "is not a primitive event type" p))
  (let ((p-nfa (nfa-plus (nfa-for-one-symbol (list p)))))
    (nfa-alphabet-set! p-nfa (alphabet))
    p-nfa))

(define (ev-and ev1 ev2 relations)
  ;; (and A B relations)
  (nfa-alphabet-set! ev1 (alphabet))
  (nfa-alphabet-set! ev2 (alphabet))
  (let loop ((nfa1 (allen (car relations) ev1 ev2))
	     (relations (cdr relations)))
    (if (not (null? relations))
	(loop (nfa-or nfa1
		      (allen (car relations) ev1 ev2))
	      (cdr relations))
	 nfa1)))

(define (ev-or ev1 ev2)
  ;; (or A B)
  (nfa-alphabet-set! ev1 (alphabet))
  (nfa-alphabet-set! ev2 (alphabet))
  (nfa-or ev1 ev2))

(define (tense-interval)
  (let* ((interval (symbol->string (gensym "i")))
	 (beg (string->symbol
	       (string-append "begin_" interval)))
	 (end (string->symbol
	       (string-append "end_"  interval)))
	 (int-nfa (nfa-concat
		   (nfa-for-one-symbol (list beg))
		   (nfa-star (nfa-empty))
		   (nfa-for-one-symbol (list end)))))
    (add-interval-fluents! (list beg end))
    (print "adding events " beg " " end)
    (nfa-alphabet-set! int-nfa (alphabet))
    int-nfa))

(define (ev-default-tense ev1)
  (let ((interval (tense-interval)))
    (nfa-alphabet-set! ev1 (alphabet))
    (weak-overlaps ev1 interval)))

(define (ev-tense ev1 relations)
  (print "tense " relations)
  (let ((interval (tense-interval)))
    (nfa-alphabet-set! ev1 (alphabet))
    (let ((nfa1 (allen (car relations) ev1 interval)))
      (print "Done with allen")
      ;(print-nfa nfa1)
      ;(print "---------------------")
      (if (not (null? (cdr relations)))
	  (ev-tense nfa1 (cdr relations))
	  nfa1))))



(define (dfa-consistent-universal-language)
  (dfa-intersection 
   ;; [] => [phi] or [not_phi]
   (dfa-complete-constraints *primitive-events* *inverses*)
   ;; [phi,not_phi] => {}
   (dfa-negation-constraints *primitive-events* *inverses*)))
				   
	 
(define (ev-not ev1)
  (nfa-alphabet-set! ev1 (alphabet))
  (let* ((dfa1 (nfa->dfa ev1))
	 ;; all the strings that contain dfa1 anywhere in them
	 (closure (weak-subsumptive-closure dfa1))
	 (L (dfa-consistent-universal-language))
	 ;; remove all the strings that subsume ev1 
	 (not-ev1 (dfa-less L closure)))
    (dfa->nfa not-ev1)))


(define (event-formula->nfa formula)
  (match-case formula
	      (((kwote not) ?A)
	       (ev-not (event-formula->nfa A)))
	      (((kwote or) ?A ?B)
	       (ev-or (event-formula->nfa A)
		      (event-formula->nfa B)))
	      (((kwote and) ?A ?B)
	       (ev-and (event-formula->nfa A) 
		       (event-formula->nfa B)
		       *default-and-relations*))
	      (((kwote and) ?A ?B ?R)
	       (ev-and (event-formula->nfa A)
		       (event-formula->nfa B)
		       R))
	      ((tense ?A)
	       (ev-default-tense (event-formula->nfa A)))
	      ((tense ?A ?R)
	       (ev-tense (event-formula->nfa A)
			 R))
	      (?primitive
	       (ev-primitive primitive))))

(define (scene-desc->list scene-desc)
  '(scene-list))

(define (parse-and-setup-alphabet! s)
  (set! *primitive-events* (parse-alphabet s))
  (set! *inverses* (negate-alphabet *primitive-events*))
  (set! *alphabet* (build-alphabet)))

(define (negate-alphabet a)
  (map (lambda (x) 
	 (string->symbol (string-append "~" (symbol->string x))))
       a))

(define (parse-alphabet s)
  (define (pa e)
    (if (member? e *internals*)
	'()
	e))
  (flatten-deep (map pa (flatten-deep s))))

(define *complete?* #f)
(define (eventlogic-main args)
;;   (view (graph (dfa-complete-constraints *primitive-events*
;; 					 *inverses*)))
;;  (view (graph (dfa-consistent-universal-language)))
  (let loop ()
    (display "> ")
    (let ((s (read)))
      (cond ((equal? s 'complete) (set! *complete?* #t) (print "Completing all dfas")(loop))
	    ((equal? s 'uncomplete) (set! *complete?* #f) (print "Not completing all dfas") (loop))
	    (else
	     (parse-and-setup-alphabet! s)
	     (let* ((nfa1 (event-formula->nfa s))
		    (dfa1 (nfa->dfa nfa1)))
	       (print-nfa nfa1)
	       (print "*************************")
					;(print (maxf length (nfa-alphabet nfa1)))
					;(map print (nfa-alphabet nfa1))
					;(view (graph nfa1))
	       (dfa-remove-never-accepting-states! dfa1)
	       (dfa-really-minimize! dfa1)
	       (if *complete?* (dfa-complete! dfa1))
	       (view (graph dfa1 s))
	       (display "Model > ")
	       (let* ((model (read))
		      (result (run-dfa dfa1 model)))
		 (if (car result)
		     (print "Accepted")
		     (print "Not accepted"))
		 (print "Input string remaining " (cdr result))
		 (loop))))))))
    
	    
