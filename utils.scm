(module utils
  (export 
    (identity x)
    (first l)
    (second l)
    (third l)
    (compose f1 f2)
    (reduce f i l)
    (find-if p l)
;;     (list-remove l item)
    (list-remove-all l item)
    (find-if-all p l)
    (list-less l1 l2)
    (nub l)
;;     (power-set l)
    (cross-product l1 l2)
    (union a b)
    (intersection a b)
;;     (sort-strings l)))
    (string-join l s)
    (to-string a)
    (temp-filename)
))
(define (identity x) x)

;; these are already defined by chicken
 (define (first l)
   (car l))
 (define (second l)
   (cadr l))
 (define (third l)
   (caddr l))

(define (compose f1 f2)
  (lambda (x)
    (f2 (f1 x))))

(define (reduce f i l)
  ;; from qobischeme
  (cond ((null? l) i)
	((null? (cdr l)) (car l))
	(else (let loop ((l (cdr l))
			 (c (car l)))
		(if (null? l) 
		    c
		    (loop (cdr l) (f c (car l))))))))

;; return the first item in l for which (p item) is true
(define (find-if p l)
  (if (null? l)
      #f
      (if (p (car l))
          (car l)
          (find-if p (cdr l)))))

;; Remove item from list l, returns a new list
(define (list-remove l item)
  (if (null? l)
      l
      (let  ((a (car l))
	     (b (cdr l)))
	(if (equal? a item)
	    b
	    (cons a (list-remove b item))))))

(define (list-remove-all l item)
  (if (null? l)
      l
      (let ((a (car l))
            (b (cdr l)))
       (if (equal? a item)
         (list-remove-all b item)
         (cons a (list-remove-all b item))))))

;; The list of items in l for which (p item) is true
(define (find-if-all p l)
  (cond ((null? l) (list))
	((p (car l)) 
	 (cons (car l) (find-if-all p (cdr l))))
	(else 
	 (find-if-all p (cdr l)))))
		    
;; Remove the items in l2 from l1
(define (list-less l1 l2)
  (if (null? l2)
      l1
      (let ((a (car l2))
	    (b (cdr l2)))
	(list-less (list-remove l1 a) b))))

;; remove the duplicates from l, returns a new list
(define (nub l)
  (let loop ((b l)	
	     (n (list)))
    (cond ((null? b) n)
	  ((member (car b) n)
	   (loop (cdr b) n))
	  (else
	   (loop (cdr b) (cons (car b) n))))))

;; Return the power set of l, eg. all the subsets of l
(define (power-set l)
  (if (null? l)
      (list (list))
      (cons (list) (%power-set l))))

(define (%power-set l)
  ;; This doesn't include the empty set in the power set,
  ;; use power-set to get the complete powerset. It also
  ;; won't work properly if you give it () for l.
  (let loop ((pow  (list l))
	     (item (car l))
	     (a    (cdr l))
	     (b    (list)))
    (cond ((and (null? a)
		(not (null? b))) ;; check this here so we don't add () to the power set
	                         ;; if we do we will end up with multiple copies of it
	   (append (%power-set (append a b)) pow))
	  ((not (null? a))
	   (loop (append (%power-set (append a b)) pow)
		 (car a)
		 (cdr a)
		 (cons item b)))
	  (else
	   pow))))

;; returns a list of pairs of a and each element of l
(define (%pair-each a l)
  (if (null? l)
      l
      (cons (list a (car l)) 
	    (%pair-each a (cdr l)))))

;; return the cross product of l1 and l2
(define (cross-product l1 l2)
  (if (null? l1) 
      l1
      (let ((prod-a (%pair-each (car l1) l2))
	    (prodRest (cross-product (cdr l1) l2)))
	(append prod-a prodRest))))

;; returns a list which contains the all the elements of a and b
;; with no duplicates
(define (union a b)
  (cond ((null? a) b)
	((member (car a) b)
	 (union (cdr a) b))
	(else 
	 (cons (car a) (union (cdr a) b)))))

(define (intersection a b)
  (let loop ((intersec (list))
	     (a a)
	     (b b))
    (if (not (null? a))
	(if (member (car a) b)
	    (loop (cons (car a) intersec)
		  (cdr a)
		  b)
	    (loop intersec (cdr a) b))
	intersec)))
		   
      
;; removes the runtime arguments to chicken and the program name, argv[0]
;; chicken arguments begin with -:
;;(define (get-args)
;;  (let loop ((args (cdr (argv))))
;;    (cond ((null? args) (list))
;;	  ((substring=? "-:" (car args) 0 0)
;;	   (loop (cdr args)))
;;	  (else args))))

;; sort a list of strings
(define (sort-strings l)
  (sort l (lambda (a b) (< (string-compare3 a b) 0))))

;; (use syntax-case) ;; chicken

;; (define-syntax for-each
;;   (syntax-rules _a
;;     ((_ (x values) bodyform1 ...)
;;      (do ((vals values (cdr vals)))
;;  	 ((null? vals))
;;        (let ((x (car vals))) bodyform1 ...)))))
;;example:
;;(for-each (x '(1 2 3)) (display x) (display ","))

; join elements in a list of strings using s as a delimter
(define (string-join l s)
  (if (null? l) 
      ""
      (let loop ((str (car l))
		 (l (cdr l)))
	(cond ((null? l) str)
	      (else (loop (string-append 
			   (string-append str s)
			   (car l))
			  (cdr l)))))))

(define (to-string a)
  (format "~a" a))

(define (temp-filename)
  (format "~a~a~a~s" 
	  (os-tmp) 
	  (file-separator)
	  "tmp" 
	  (random 65535)))
