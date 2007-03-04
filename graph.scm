;; draw graphs of state machines
;; outputs a file in the dot format
;; They can be compiled using:
;; dot -Tpng -o out.png  file.dot
(module graph
	(include "nfa.sch")
	(include "dfa.sch")
	;(main main-graph)
	(import 
	 (nfa "nfa.scm")
	 (dfa "dfa.scm")
	 (utils "utils.scm"))
	(export 
	 (show-graph graph)
	 (graph state-machine)
	 (graph-to-file state-machine file)))

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
  (format "\t\"~s\" -> \"~s\" [label=\"~s\"];\n" 
	  (car transition)
	  (caddr transition)
	  (cadr transition)))

(define dot-format 
"digraph G {
    rankdir=LR;
    node[shape=circle style=invis]
    ~a;
    node[shape=doublecircle style=solid];
    ~a;
    node [shape=circle]
    ~a
}")

(define (to-string x)
  (format "\"~s\"" x))

(define (graph-dfa dfa)
  (let* ((final-states (string-join 
			(map to-string (dfa-final-states dfa)) " "))
	(pre-start 1)
	(transitions (map graph-transition
			  (append (dfa-transition-list dfa)
				  ;; add dummy transition to the starting state
				  (list 
				   (list 
				    pre-start
				    ""
				    (dfa-start-state dfa)))))))
    (format dot-format 
	    pre-start
	    final-states 
	    (string-join transitions ""))))


(define (graph-nfa nfa)
  (let ((final-states (string-join 
		       (map to-string (nfa-final-states nfa)) " "))
	(transitions (list)))
    (let loop0 ((trans (nfa-transition-list nfa)))
      (cond ((not (null? trans))
	     (let loop1 ((edges (third (car trans))))
	       (cond ((not (null? edges))
		      (set! 
		       transitions
		       (cons (graph-transition
			      (list (first (car trans))
				    (second (car trans))
				    (car edges)))
			     transitions))
		      (loop1 (cdr edges)))))
	     (loop0 (cdr trans)))))
    ;; add dummy transition to the starting state
    (set! transitions (cons (graph-transition
			     (list 1 "" (nfa-start-state nfa)))
			    transitions))
    (format dot-format 1 final-states (string-join transitions ""))))

;; Generate a png from the graph
;; using dot. Display the graph using eog.
(define (show-graph graph)
  (let* ((tempfile (temp-filename))
	 (proc (run-process "dot" "-Tpng" "-o" tempfile :input :pipe))
	 (proc-input (process-input-port proc)))
    (fprint proc-input graph)
    (flush-output-port proc-input)
    (close-output-port proc-input)
    (process-wait proc)
    (run-process "eog" tempfile)))

(define (main-graph argv)
  (show-graph (graph test-dfa1))
  (print (graph test-dfa1))
  ;(print (graph test-nfa3))
  ;(show-graph (graph test-dfa1))
  ;(show-graph (graph (complete test-dfa1)))
  ;(show-graph (graph (inverse test-dfa1)))
  (print-nfa test-nfa3)
  (print-dfa (nfa->dfa test-nfa3))
  ;(show-graph (graph (complete (nfa->dfa test-nfa3))))
  ;(show-graph (graph test-nfa3)))
)





