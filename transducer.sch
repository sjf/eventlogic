;; Finite State Transducer

;; transitions list should be a list of the form
;; ((current-state (upper-symbol lower-symbol) next-state) 
;;   ...)
(define-struct transducer
  start-state
  transition-list
  final-states
  upper-alphabet
  lower-alphabet)
