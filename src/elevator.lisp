(in-package :cl-user)
(defpackage elevator
  (:use :cl))
(in-package :elevator)

;;;;parameters
(defparameter *floor-number* nil)
(defparameter *elevator-capacity* nil)
(defparameter *elevator-number* nil)
(defparameter *time-boarding* nil)
(defparameter *time-between-floor* nil)

(defparameter *simulation-speed* nil)

;;;;functions

;;number of elevator 
(defun set-elevator-number (n)
  (cond
    ((not (typep n 'integer)) (princ "error: elevator number must be an integer"))
    ((< n 1) (princ "error: elevator number must be a strictly positiv integer"))
    (t (setf *elevator-number* n))))

;;number of person fitting in the elevator
(defun set-elevator-capacity-int (n)
  (cond
    ((< n 1) (princ "error: elevator capacity must be a strictly positiv integer"))
    (t (setf *elevator-capacity* (list n)))))

(defun set-elevator-capacity-list (l)
  (cond
    ((not (every #'(lambda (n) (> n 0)) l)) 
     (princ "error: elevator capacity must be a strictly positiv integer")) 
    (t (setf *elevator-capacity* l))))

(defun set-elevator-capacity (x)
  (cond
    ((typep x 'integer) (set-elevator-capacity-int x))
    ((typep x 'list) (set-elevator-capacity-list x))
    (t (princ "error: elevator capacity must be a integer, or a list of integer"))))

;;
(defun set-floor-number (n)
  (cond
    ((and (typep n 'integer) (> n 0)) (setf *floor-number* n))
    (t (princ "error: elevator capacity must be a strictly positiv integer"))))
