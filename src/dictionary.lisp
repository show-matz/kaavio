#|
#|ASD|#				(:file "dictionary"                :depends-on ("kaavio"))
#|EXPORT|#				;dictionary.lisp
 |#


(in-package :kaavio)

#|
#|EXPORT|#				:*dict-mute-history*
 |#
(defparameter *dict-mute-history* nil)

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
   (history-top		:initform   0 :initarg :hist-top)	; list
   (history-size	:initform   0 :initarg :hist-size)	; integer
   (map				:initform nil :initarg :map)))		; hashtable

(defun dict-create (history-max-count)
  (make-instance 'dictionary
				 :hist-max  history-max-count
				 :hist-top  nil
				 :hist-size 0
				 :map       (make-hash-table :test 'eq)))
				 

(defun dict-register (dict entity)
  (with-slots (history-max history-top history-size map) dict
	(let ((id (slot-value entity 'id)))
	  (when id
		(when (or (eq id :canvas) (gethash id map))
		  (throw-exception "ID '~A' has already exist in dictionary." id))
		(setf (gethash id map) entity)))
	(unless *dict-mute-history*
	  (if (= history-size history-max)
		  (setf history-top (cons entity (nbutlast history-top 1)))
		  (progn
			(incf history-size)
			(setf history-top (cons entity history-top))))))
  entity)

(defun dict-get-entity (dict id)
  (labels ((history-kwd-p (id)
			 (let ((str (symbol-name id)))
			   (when (char= #\$ (char str 0))
				 (multiple-value-bind (idx end)
					 (parse-integer str :start 1 :junk-allowed t)
				   (when (= end (length str))
					 idx))))))
	(with-slots (history-max history-top history-size map) dict
	  (let ((index (history-kwd-p id)))
		(if (null index)
			(gethash id map)
			(if (or (< index 1) (< history-size index))
				(throw-exception "ID '~A' is out of history range." id)
				(nth (1- index) history-top)))))))

