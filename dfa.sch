;; dfa.sch
(define-struct dfa
  start-state
  transition  
  transition-list
  final-states
  alphabet)
  ;; transition is the transition function, takes current state and symbol
  ;; as arguments and returns the next state
