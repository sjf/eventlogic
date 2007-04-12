(module 
 eventlogic
 (main event-logic-main)
 (import (dfa "dfa.scm")
	 (nfa "nfa.scm")
	 (utils "utils.scm")
	 (regex "regex.scm")
         (graph "graph.scm")
	 (transducer "transducer.scm")
         (snapshots "snapshots.scm")))

(define (events->transducer events)
  '(trans))
(define (scene-desc->list scene-desc)
  '(scene-list))

