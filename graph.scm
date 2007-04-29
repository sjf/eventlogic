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
	 (view graph)
	 (graph state-machine)
	 (graph-to-file state-machine file)))

(define *image-viewer* "eog")
(define *dot* "dot")

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
  (format "\t\"~a\" -> \"~a\" [label=\"~a\"];\n" 
	  (first transition)
	  (third transition)
	  (second transition)))

(define dot-format 
"digraph G {
    rankdir=LR;
    node[shape=circle style=invis]
    ~a;
    ~a
    node[shape=circle style=solid]
    ~a
}")
(define dot-final-states
  "node[shape=doublecircle style=solid]
    ~a;")

(define (string-quote x)
  (format "\"~a\"" x))

(define (graph-dfa dfa)
  (let* ((final-states (if (null? (dfa-final-states dfa))
			   ""
			   (format dot-final-states
				   (string-join 
				    (map string-quote (dfa-final-states dfa)) " "))))
	(pre-start (gensym "dummy"))
	(transitions (map graph-transition
			  (append (dfa-transition-list dfa)
				  ;; add dummy transition to the starting state
				  (list 
				   (list 
				    pre-start
				    ""
				    (dfa-start-state dfa)))))))
    ;; fill parameters into string
    (format dot-format 
	    pre-start
	    final-states 
	    (string-join transitions ""))))


(define (graph-nfa nfa)
  (let* ((final-states (if (null? (nfa-final-states nfa))
			   ""
			   (format dot-final-states
				   (string-join 
				    (map string-quote (nfa-final-states nfa)) " "))))
	 (pre-start (gensym "dummy"))
	 (trans-list
	  (cons
	   ;; dummy transition to the starting state
	   (graph-transition
	    (list pre-start "" (nfa-start-state nfa)))
	   ;; make a graph edge each transition
	   (map (lambda (trans)
		  (graph-transition trans))
		(nfa-transition-list nfa))))
	 (transitions (string-join trans-list "")))
    ;; fill parameters into string
    (format dot-format pre-start final-states transitions)))

;; Generate a png from the graph
;; using dot. Display the graph using eog.
(define (view graph)
  (let* ((tempfile (temp-filename))
	 (proc (run-process *dot* "-Tpng" "-o" tempfile :input :pipe))
	 (proc-input (process-input-port proc)))
    (fprint proc-input graph)
    (flush-output-port proc-input)
    (close-output-port proc-input)
    (process-wait proc)
    (run-process *image-viewer* tempfile error: "/dev/null")))

(define (main-graph argv)
  (view (graph test-dfa1))
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





