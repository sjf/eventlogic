;; nfa.sch
(define-struct nfa
  alphabet ;; you need this to get the dfa
  states   ;; this doesn't do anything right now
  start-state
  transition
  transition-list
  final-states)