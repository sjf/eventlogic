(module ex)
(with-trace 0 'bob 
	    (define l '(1 2 3))
	    (define l2 '(b o b))
	    (print l)
	    (print l2)
	    (let loop ((c 0))
	      (print c)
	      (if (< c 5)
		  (loop (+ c 1)))))

