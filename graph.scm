;; draw graphs of state machines
;; outputs a file in the dot format
;; They can be compiled using:
;; dot -Tpng -o out.png  file.dot
(module graph
	(include "nfa.sch")
	(include "dfa.sch")
	(import 
	 (nfa "nfa.scm")
	 (dfa "dfa.scm")
	 (utils "utils.scm")))
;	(declare (uses srfi-13)) ; string library

(define (graph state-machine)
  (cond ((nfa? state-machine)
	 (graph-nfa state-machine))
	((dfa? state-machine)
	 (graph-dfa state-machine))
	(else
	 (error "graph" "Can only produce a graph for nfa or dfa" state-machine))))

(define (graph-to-file state-machine filename)
  (call-with-output-file
      filename
    (lambda () (graph state-machine))))

(define (graph-transition transition)
  (format "\t~s -> ~s [label=\"~s\"];\n" 
	  (car transition)
	  (caddr transition)
	  (cadr transition)))

(define (graph-dfa dfa)
  (print "digraph G{\n\trankdir=LR;")
  (display "\tnode[shape=doublecircle];") 
  (display (string-join 
	    (map symbol->string (dfa-final-states dfa)) " "))
  (print ";")
  (print "\tnode [shape=circle]")
  (map (compose graph-transition display)
       (dfa-transition-list dfa))
  (print "}"))

(define (graph-nfa nfa)
  (print-nfa nfa (current-output-port))
  (print "digraph G{\n\trankdir=LR;")
  (display "\tnode[shape=doublecircle];") 
  (display (string-join 
	    (map symbol->string (nfa-final-states nfa)) " "))
  (print ";")
  (print "\tnode [shape=circle]")
  (let loop0 ((trans (nfa-transition-list nfa)))
    (cond ((not (null? trans))
	   (let loop1 ((edges (third (car trans))))
	     (cond ((not (null? edges))
		    (display (graph-transition
			      (list (first (car trans))
				    (second (car trans))
				    (car edges))))
		    (loop1 (cdr edges)))))
	   (loop0 (cdr trans)))))
  (print "}"))

(if #t
    (begin
      (graph test-dfa1)
      (graph test-nfa3)))




