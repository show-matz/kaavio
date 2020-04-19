#|
#|ASD|#				(:file "dictionary"                :depends-on ("cl-diagram"))
#|EXPORT|#				;dictionary.lisp
 |#


(in-package :cl-diagram)

;;------------------------------------------------------------------------------
;;
;; dictionary
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:dictionary
 |#
(defclass dictionary ()
  ((history-max		:initform   0 :initarg :hist-max)	; integer
   (history-arr		:initform nil :initarg :hist-arr)	; array
   (history-size	:initform   0 :initarg :hist-size)	; integer
   (history-top		:initform   0 :initarg :hist-top)	; integer
   (map				:initform nil :initarg :map)))		; hashtable

(defun dict-create (history-size)
  (make-instance 'dictionary
				 :hist-max  history-size
				 :hist-arr  (make-array history-size :initial-element nil)
				 :hist-size 0
				 :hist-top  0
				 :map       (make-hash-table :test 'eq)))
				 

(defun dict-register (dict id entity)
  (with-slots (history-max history-arr
			   history-size history-top map) dict
	(when id
	  (when (gethash id map)
		(throw-exception "ID '~A' has already exist in dictionary." id))
	  (setf (gethash id map) entity))
	(decf history-top)
	(when (< history-top 0)
	  (setf history-top (1- history-max)))
	(setf (svref history-arr history-top) entity)
	(when (< history-size history-max)
	  (incf history-size))
	entity))

(defun dict-get-entity (dict id)
  (labels ((history-kwd-p (id)
			 (let ((str (symbol-name id)))
			   (when (char= #\$ (char str 0))
				 (multiple-value-bind (idx end)
					 (parse-integer str :start 1 :junk-allowed t)
				   (when (= end (length str))
					 idx))))))
	(let ((index (history-kwd-p id)))
	  (with-slots (history-max history-arr
							   history-size history-top map) dict
		(if (null index)
			(gethash id map)
			(if (or (< index 1) (< history-size index))
				(throw-exception "ID '~A' is out of history range." id)
				(svref history-arr
					   (mod (+ history-top (1- index)) history-max))))))))



