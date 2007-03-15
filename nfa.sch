;; nfa.sch
(define-struct nfa
  alphabet ;; you need this to get the dfa
  states   
  start-state
  transition-list
  final-states)
