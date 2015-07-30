(in-package :cl-user)
(defpackage elevator
  (:nicknames #:el)
  (:use :cl
	:bordeaux-threads))
(in-package :elevator)

;;;;parameters
(defparameter *floor-number* nil)
(defparameter *elevator-capacity* nil)
(defparameter *elevator-number* nil)
(defparameter *time-boarding* nil)
(defparameter *time-between-floor* nil)

(defparameter *simulation-speed* nil)

(defparameter *elevator-call* nil) ;alist

(defparameter *lock* (bt:make-lock "lock"))

(defparameter *internal-time* nil)
(defparameter *time-max-for-simulation* nil)
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
    (t (princ "error: floor number must be a strictly positiv integer"))))

;;
(defun set-time-boarding (n)
  (cond
    ((and (numberp n) (>= n 0))  (setf *time-boarding* n))
    (t (princ "error: time boarding must be positiv number"))))
;;
(defun set-time-between-floor (n)
  (cond 
    ((and (numberp n) (>= n 0)) (setf *time-between-floor* n))
    (t (princ "error: time between floor must be a positiv number"))))
;;
(defun set-internal-time (n)
  (cond 
    ((and (numberp n) (>= n 0)) (setf *internal-time* n))
    (t (princ "error: internal time must be a positiv number"))))

(defun reset-internal-time ()
  (set-internal-time 0))

(defun add-n-internal-time (n)
  (incf *internal-time* n))

(defun incr-internal-time ()
  (add-n-internal-time 1))
;;
(defun set-time-max-for-simulation (n)
  (cond
    ((and (numberp n) (>= n 0)) (setf *time-max-for-simulation* n))
    (t (princ "error: internal time must be a positiv number"))))
;;
(defun set-simulation-speed ()
  (setf *simulation-speed* 1))

(defun accelerate-simulation-speed ()
  (if (< *simulation-speed* 8)
      (setf *simulation-speed* (* *simulation-speed* 2))
      (print "wont go faster")))

(defun decelerate-simulation-speed ()
  (if (> *simulation-speed* 1/8)
      (setf *simulation-speed* (/ *simulation-speed* 2))
      (print "wont go slower")))

;;

(defun setup-time-internal (&key 
		     (internal-time 0) 
		     (time-max-for-simulation 600))
  (set-internal-time internal-time)
  (set-time-max-for-simulation time-max-for-simulation)
  t)

;;
(defun setup (&key 
		(time-between-floor 1) 
		(time-boarding 2) 
		(elevator-number 1) 
		(elevator-capacity 16)
		(floor-number 5)
		(internal-time 0)
		(time-max-for-simulation 600))
  "Gives a value to every global variable that needs it
for the program to run."
  (set-time-between-floor time-between-floor)
  (set-time-boarding time-boarding)
  (set-elevator-number elevator-number)
  (set-elevator-capacity elevator-capacity)
  (set-floor-number floor-number)
  (setup-time-internal :internal-time internal-time :time-max-for-simulation time-max-for-simulation)
  (set-simulation-speed)
  t)

;;

(defun elevator-call-on-nth-p (floor)
  (not (or (not (assoc floor *elevator-call*)) (< (cdr (assoc floor *elevator-call*)) 0))))

(defun add-elevator-call (floor time)
  (cond 
    ((not (assoc floor *elevator-call*)) 
     (push (cons floor time) *elevator-call*))
    ((< (cdr (assoc floor *elevator-call*)) 0)
     (setf (cdr (assoc floor *elevator-call*)) time)))
  *elevator-call*)

(defun remove-elevator-call (floor)
  (cond
    ((not (assoc floor *elevator-call*))
     (push (cons floor -1) *elevator-call*))
    ((>= (cdr (assoc floor *elevator-call*)) 0)
     (setf (cdr (assoc floor *elevator-call*)) -1)))
    *elevator-call*)

;;
(defun generate-elevator-call-floor-n (floor time)
  (when (> 3 (random 100)) 
    (add-elevator-call floor time)))

(defun generate-elevator-call ()
  "Generate a alist, with a maximum of *number-floor* + 1 entry.
Each value is either -1 if the floor didnt make an elevator call,
or a positiv number representing the time of the call (using *internal-time*)."
  (bt:thread-yield)
  (loop 
     for i from *internal-time* 
     to *time-max-for-simulation* 
     do
       (dotimes (j *floor-number*)
	  (generate-elevator-call-floor-n j i))
       (format t "generate-elevator-call:~%~A~%" *elevator-call*)))


       
;;
(defun make-elevator ()
  "Closure that create an elevator.
You can use the function like this: 
-(defparameter parameter-name (make-elevator))
And then you can give it command like this:
-(funcall parameter-name 'command-name)
The command available are:
-move-up 
-move-down
-board
-which-floor
-list-command to return a list of all the previous command (except list-command itself)."
  (let ((floor 0)
	(command ()))
    #'(lambda (operation)
    (ecase operation
      (move-up
       (progn
	 (when (< (1+ floor) *floor-number*)
	   (incf floor 1)
	   (setf command (cons 'mu command))
	   (sleep *time-between-floor*))))
      (move-down
       (progn
	 (when (>= (1- floor) 0)
	   (decf floor 1)
	   (setf command (cons 'md command))
	   (sleep *time-between-floor*))))
      (board
       (progn
	 (remove-elevator-call floor)
	 (setf command (cons 'b command))
	 (sleep *time-boarding*)))
      (which-floor
       (setf command (cons 'wf command))
       floor)
      (list-command
       command)))))


;;

(defun action-move-to (elevator floor)
  (let ((f (funcall elevator 'which-floor)))
    (format t "going to ~D~%" floor)
    (cond
      ((< floor f) 
       (dotimes (i (- f floor))
	 (funcall elevator 'move-down)))
      ((> floor f)
       (dotimes (i (- floor f))
	 (funcall elevator 'move-up)))
      (t (print "fn action-move-to: not moving")))))

(defun action-board (elevator)
  (format t "Boarding~%")
  (funcall elevator 'board))

(defun action-list-command (elevator)
  (reverse (funcall elevator 'list-command)))
    

(defun naif-elevator ()
  (bt:thread-yield ) ;;allow others thread to run, necessary?
  (let ((el (make-elevator)))
    (loop 
       for i from 0 to 70
	 do
	 (loop 
	    for goal from *floor-number* downto 0
	    do (when (elevator-call-on-nth-p goal)
		 (action-move-to el goal)
		 (action-board el)
		 (action-move-to el 0)
		 (action-board el)
		 (format t "done~%")))
	 (sleep 0.10))
    (action-list-command el)))



;;threads
(defun setup-thread ()
  (let ((g (bt:make-thread #'generate-elevator-call 
			   :name "generate-calls" 
			   :initial-bindings `((*standard-output* . ,*standard-output*)
					       (*error-output* . ,*error-output*))))
	(ne (bt:make-thread #'naif-elevator
			    :name "elevator"
			    :initial-bindings `((*standard-output* . ,*standard-output*)
						(*error-output* . ,*error-output*)))))
    (print "sleeping")
    (sleep 60)
    (format t "~A~%" ne)
    (format t "~A~%" g) 
    (bt:join-thread ne)
    (bt:join-thread g)))

