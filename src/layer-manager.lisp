#|
#|ASD|#				(:file "layer-manager"             :depends-on ("cl-diagram"
#|ASD|#																"writer"))
#|EXPORT|#				;layer-manager.lisp
 |#

(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; layer-manager
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#				:layer-manager
 |#
(defclass layer-manager ()
  ((current  :initform nil :initarg :current)	; (or nil keyword symbol)
   (counter  :initform   0 :initarg :counter)	; integer
   (map      :initform nil :initarg :map)))		; hashtable
			 
(defun layer-create-manager ()
  (make-instance 'layer-manager
				 :current nil
				 :counter 0
				 :map (make-hash-table :test 'eq)))

(defun layer-register (layers name display)
  (unless (keywordp name)
	(throw-exception "Invalid name parameter for layer."))
  (unless (or (eq display :inline) (eq display :none))
	(throw-exception "Display parameter for layer must be :inline or :none."))
  (with-slots (counter map) layers
	(when (gethash name map)
	  (throw-exception "Layer '~A' has already exists." name))
	(setf (gethash name map) (cons display counter))
	(incf counter)))

(defun layer-get-display (layer-mgr name)
  (with-slots (map) layer-mgr
	(car (gethash name map))))

(defun layer-get-priority (layer-mgr name)
  (with-slots (map) layer-mgr
	(let ((pri (cdr (gethash name map))))
	  (or pri most-positive-fixnum))))

(defun layer-change (layer-mgr new-layer writer)
  (with-slots (current) layer-mgr
	(unless (eq current new-layer)
	  (when current
		(writer-decr-level writer)
		(writer-write      writer "</g>"))
	  (when new-layer
		(let ((display (layer-get-display layer-mgr new-layer)))
		  (unless display
			(throw-exception "Layer '~A' is not defined." new-layer))
		  (writer-write writer "<!-- layer " new-layer " -->")
		  (writer-write writer "<g id='" new-layer "' display='" display "'>")
		  (writer-incr-level writer)))
	  (setf current new-layer))))

