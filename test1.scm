;; (define f (lambda (x) x))
;; (define g (lambda (y) y))
;; (define l (list f g))
;; (print f)
;; (print l)
;; (print ,(quote f))


;; (map print l)
;; (map (lambda (a) (print a)) l)
;; (map (lambda (a) (print , a)) l)

;; (let loop (l list)
;;   (if (not (null? l))
;;       (let ((item) (car list))
;; 	...
;; 	(loop (cdr l)))))

(use syntax-case)


;; (define-syntax for-each
;;   (syntax-rules ()
;;     ((_ (item list) ...)
;;      ((let* ((item (car list)))
;; 	(print item)
;; 	...)))))

;; (define-syntax for-each
;;   (syntax-rules (lambda)
;;     ((_ (lambda (x) bodyform1 ...) values)
;;      (do ((vals values (cdr vals)))
;; 	 ((null? vals))
;;        (let ((x (car vals))) bodyform1 ...)))))
;; 

(define-syntax for-each
  (syntax-rules (lambda)
    ((_ (x values) bodyform1 ...)
     (do ((vals values (cdr vals)))
	 ((null? vals))
       (let ((x (car vals))) bodyform1 ...)))))
     

(for-each (item (list 1 2 3 4)) (display item) (display "="))
;(for-each (lambda (n) (display n) (display "=")) (list 1 2 3))
;;      ((display list
;;       ...))))

;(for-each (item (list 1 2 3 4)))
;; (define-syntax for-each
;;   (syntax-rules ()
;;     ((_ (item list) ...)
;;      (let* ((loop (lambda l
;; 		    (if (not (null? l))
;; 			(let ((item) (car list))
;; 			  ...
;; 			  (loop (cdr l)))))))
;;        (loop list)))))
     

 	  
