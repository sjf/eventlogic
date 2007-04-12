(module 
 demo
 (main demo-main)
 (import (graph "graph.scm") 
	 (eventlogic "eventlogic.scm")))

(define (main args)
  ((let* ((primtive-events (read))
	  (event-formulas (read))
	  (scene-desc (read))
	  (events-trans (events->transducer events))
	  (scene-list (scene-desc->list scene-desc))
	  (results (run-trans-all-matches event-trans scene-list)))
     (print results))))
	  
  
