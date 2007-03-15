;; nfa.sch
(define-struct nfa
  alphabet ;; you need this to get the dfa
  states   
  start-state
  transition
  transition-list
  final-states)
