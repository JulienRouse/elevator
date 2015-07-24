(in-package :cl-user)
(defpackage elevator-test
  (:use :cl
        :elevator
	:prove))
(in-package :elevator-test)

;; NOTE: To run this test file, execute `(asdf:test-system :elevator)' in your Lisp.
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))


(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	    (mapc #'(lambda ,parms ,@body)
		  ,@(map0-n #'(lambda (n)
				`(nthcdr  ,n ,src))
			    (1- (length parms))))))))


;;TODO ca ne marche pas du tout
(defmacro create-subtest (function-name &rest rest)
  (progn
    (dolist (x rest)
    `((first x) '(,function-name (,#'second ,x)) (third x)))))

(defmacro tmp (fn )
  `(subtest ,(concatenate 'string "Testing " (string fn))))


(defmacro tmp2 (fn predicate param expected)
  `(,predicate (,fn ,param) ,expected))

(defmacro tmp3 (fn predicate param expected)
  `(subtest ,(concatenate 'string "Testing " (string fn))
     (,predicate (,fn ,param) ,expected)
     (,predicate (,fn ,param) ,expected)))

(defmacro tmp4 (fn &rest rest)
  `(subtest ,(concatenate 'string "Testing " (string fn))
    (dolist (x ,rest)
      (let ((f (first x))
	    (s (second x))
	    (tt (third x)))
	`(tmp2 fn ,f ,s ,tt))))))  
    
(defmacro tmp5 (fn &rest rest)
  `(do-tuples/o (a b c) ,@rest
     (funcall a (funcall ,fn b) c)))

;(create-subtest (elevator:elevator (is 2 2) (is 3 3)
;

(defun run-test ()
  (subtest "Testing set-elevator-number"
    (is (elevator::set-elevator-number 2) 2)
    (is-print (elevator::set-elevator-number "ok") "error: elevator number must be an integer")
    (is-print (elevator::set-elevator-number -1) "error: elevator number must be a strictly positiv integer"))
  (subtest "Testing set-elevator-capacity"
    (is (elevator::set-elevator-capacity 2) '(2))
    (is (elevator::set-elevator-capacity '(1 2 3)) '(1 2 3))
    (is-print (elevator::set-elevator-capacity #(1 2 3)) "error: elevator capacity must be a integer, or a list of integer")
    (is-print (elevator::set-elevator-capacity -2) "error: elevator capacity must be a strictly positiv integer")
    (is-print (elevator::set-elevator-capacity '(1 2 -1)) "error: elevator capacity must be a strictly positiv integer")))
  

(plan 2)

(run-test)

(finalize)
