(module
 transducer
 (main transducer-main)
 (include "transducer.sch")
 (import (dfa "dfa.scm")
         (nfa "nfa.scm")
         (utils "utils.scm")
         (regex "regex.scm")
         (graph "graph.scm")
         (snapshots "snapshots.scm")))

(define (transducer-main)
  (trans
