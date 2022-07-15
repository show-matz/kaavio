#|
#|ASD|#				(:file "cl-diagram")
#|EXPORT|#				;cl-diagram.lisp
 |#

(provide :cl-diagram)

(defpackage		:cl-diagram
  (:use			:common-lisp)
  (:nicknames	:diagram)
  (:export  	;--------------- BEGIN EXPORT
				;arc.lisp
				:arc
				;balloon.lisp
				:*default-balloon-round*
				:*default-balloon-align*
				:*default-balloon-valign*
				:*default-balloon-margin*
				:*default-balloon-filter*
				:balloon
				;binutil.lisp
				;block-arrow.lisp
				:*default-block-arrow-filter*
				:block-arrow
				:block-arrow1
				:block-arrow2
				;brace.lisp
				:*default-brace-filter*
				:brace
				;canvas.lisp
				:canvas
				:make-canvas
				:copy-canvas
				:canvas-p
				:canvas-topleft
				:canvas-left
				:canvas-top
				:canvas-right
				:canvas-bottom
				:canvas-width
				:canvas-height
				:canvas-fix-point
				:with-canvas
				:with-subcanvas
				;circle.lisp
				:circle-connect-point
				:circle
				;cl-diagram.lisp
				:make-id
				:fix-name
				:exception
				:caution
				:throw-exception
				:throw-caution
				:type-assert
				:chk-type
				:format-string
				:check-member
				:check-object
				:check-keywords
				:check-numbers
				:write-when
				:it
				:with-dictionary
				:attr
				:escape-characters
				:to-property-strings
				:to-style-strings
				:check
				:rgb
				;colormap.lisp
				;connector.lisp
				:resolve-connector-points
				:connector
				:connect
				;constants.lisp
				:*default-link-target*
				:*default-endmark-1*
				:*default-endmark-2*
				:*default-endmark-type*
				:*default-endmark-size*
				:*default-label-position*
				:*default-label-font*
				:*default-label-offset*
				:*default-connector-style*
				:*default-connector-spacing*
				:*default-rectangle-rx*
				:*default-rectangle-ry*
				:*default-text-align*
				:*default-paragraph-align*
				:*default-paragraph-valign*
				:*default-history-count*
				;create-svg.lisp
				:create-svg
				:register-entity
				:layer
				:width
				:height
				:diagram
				;cylinder.lisp
				:*default-cylinder-filter*
				:cylinder
				;defs.lisp
				:defs
				;dictionary.lisp
				:dictionary
				;document.lisp
				:*default-document-align*
				:*default-document-valign*
				:*default-document-filter*
				:document
				;ellipse.lisp
				:ellipse-connect-point
				:ellipse
				;endmark-info.lisp
				:endmark-info
				:make-endmark
				;entity.lisp
				:entity
				:write-header
				:draw-entity
				:pre-draw
				:post-draw
				:entity-composition-p
				:check-and-draw-local-entity
				;explosion.lisp
				:*default-explosion-filter*
				:explosion1
				:explosion2
				;fill-info.lisp
				:*default-fill*
				:fill-info
				:make-fill
				:with-fill
				;filter.lisp
				:*default-line-filter*
				:*default-shape-filter*
				:filter
				:write-filter
				;folder.lisp
				:*default-folder-tabwidth*
				:*default-folder-tabheight*
				:*default-folder-align*
				:*default-folder-valign*
				:*default-folder-margin*
				:*default-folder-filter*
				:folder
				;font-info.lisp
				:*default-font*
				:*default-font-fill*
				:*default-font-stroke*
				:*default-font-filter*
				:font-info
				:make-font
				:with-font
				:font-calc-textarea
				;grid.lisp
				:grid
				;group.lisp
				:group
				:group-get-canvas
				:draw-group
				:draw-canvas-frame
				;image.lisp
				:image
				;label-info.lisp
				:label-info
				:draw-label-with-point
				:draw-label
				:make-label
				;layer-manager.lisp
				:layer-manager
				;line.lisp
				:line
				;link-info.lisp
				:link-info
				:write-link-open
				:write-link-close
				:make-link
				;mathutil.lisp
				:math/len2
				:math/len4
				:math/sin1
				:math/sin2
				:math/sin3
				:math/sin4
				:math/sin5
				:math/cos1
				:math/cos2
				:math/cos3
				:math/cos4
				:math/cos5
				;memo.lisp
				:*default-memo-dog-ear*
				:*default-memo-align*
				:*default-memo-valign*
				:*default-memo-margin*
				:*default-memo-filter*
				:memo
				;paragraph.lisp
				:paragraph
				;path.lisp
				:path
				;pathutil.lisp
				;point.lisp
				:make-point
				:copy-point
				:point-p
				:point-absolute-p
				:point-relative-p
				:point-x
				:point-y
				:point+
				:point-
				:point/x+
				:point/y+
				:point/xy+
				:x+
				:y+
				:xy+
				:point-distance
				:with-point
				;polygon.lisp
				:polygon
				;raw-svg.lisp
				:raw-svg
				;rectangle.lisp
				:rectangle
				:rect
				;shadow-filter.lisp
				:drop-shadow
				:glow-shadow
				;shape.lisp
				:rectangle-connect-point
				:shape
				:shape-width
				:shape-height
				:shape-topleft
				:shape-top
				:shape-topright
				:shape-left
				:shape-center
				:shape-right
				:shape-bottomleft
				:shape-bottom
				:shape-bottomright
				:shape-get-subcanvas
				:shape-cc-center
				:shape-connect-point
				;stencil.lisp
				:*include-paths*
				:*stencil-suffix*
				:reset-stencil-load-cache
				:load-stencil
				;stroke-info.lisp
				:*default-stroke*
				:stroke-info
				:make-stroke
				:with-stroke
				;stylesheet.lisp
				:style
				:stylesheet
				;table.lisp
				:table
				:with-table-cell
				;text-shape.lisp
				:text-shape
				:text-shape-calc-size
				:text-shape-paragraph-area
				;text.lisp
				:write-text-tag
				:text
				;textbox.lisp
				:*default-textbox-rx*
				:*default-textbox-ry*
				:*default-textbox-align*
				:*default-textbox-valign*
				:*default-textbox-margin*
				:*default-textbox-filter*
				:textbox
				;use.lisp
				:use
				;writer.lisp
				:writer-write
				:writer-incr-level
				:writer-decr-level
				:writer-close
				:buffer-writer
				:create-svg-writer
				;--------------- END EXPORT
))


(defpackage 	:cl-diagram-user
  (:use			:common-lisp
				:cl-diagram)
  (:nicknames	:diagram-user))


(in-package :cl-diagram)


;;------------------------------------------------------------------------------
;;
;; string manipulation utilities
;;
;;------------------------------------------------------------------------------
;;(locally (declare (optimize speed))
;;  (defun left (str len)
;;	(declare (type string str))
;;	(declare (type fixnum len))
;;	(subseq str 0 len)))
;;
;;(locally (declare (optimize speed))
;;  (defun right (str len)
;;	(declare (type string str))
;;	(declare (type fixnum len))
;;	(subseq str (- (length str) len))))

(locally (declare (optimize speed))
  (defun string/mid (str idx &optional (len -1))
	(declare (type string str))
	(declare (type fixnum idx len))
	(if (< len 0)
		(subseq str idx)
		(subseq str idx (+ idx len)))))

(locally (declare (optimize speed))
  (defun string/replace (target str1 str2)
	(declare (type string target str1 str2))
	(let ((len1 (length str1))
		  (len2 (length str2))
		  (target-len (length target)))
	  (declare (type fixnum len1 len2 target-len))
	  (labels ((imp (idx acc)
				 (declare (type fixnum idx))
				 (if (< (- target-len idx) len1)
					 (push (subseq target idx) acc)
					 (let ((ret (search str1 target :start2 idx)))
					   (if (null ret)
						   (push (subseq target idx) acc)
						   (progn
							 (when (< idx ret)
							   (push (subseq target idx ret) acc))
							 (push str2 acc)
							 (imp (+ ret len1) acc)))))))
		(if (and (= len1 1) (= len2 1))
			(substitute (aref str2 0) (aref str1 0) target)
			(apply #'concatenate 'string (nreverse (imp 0 nil))))))))

; example : (string/split "abc,def,ghi" #\,) => ("abc" "def" "ghi")
(locally (declare (optimize speed))
  (defun string/split (line separator)
	(declare (type string line))
	(let ((acc nil))
	  (do ((idx1 0)
		   (idx2 0))
		  ((null idx1) (nreverse acc))
		(setf idx2 (position separator line :start idx1))
		(if (null idx2)
			(push (string/mid line idx1) acc)
			(progn
			  (push (string/mid line idx1 (- idx2 idx1)) acc)
			  (incf idx2)))
		(setf idx1 idx2)))))

;;------------------------------------------------------------------------------
;;
;; utilities from 'on lisp' and their variant.
;;
;;------------------------------------------------------------------------------
(defun onlisp/mkstr (&rest args)
  (with-output-to-string (s)
	(dolist (a args)
	  (princ a s))))

(defun onlisp/symb (&rest args)
  (values (intern (apply #'onlisp/mkstr args) 'cl-diagram)))

(defun onlisp/keysymb (&rest args)
  (values (intern (apply #'onlisp/mkstr args) :keyword)))

(defun onlisp/flatten (x &optional (acc nil))
  (cond ((null x) acc)
		((atom x) (cons x acc))
		(t (onlisp/flatten (car x) (onlisp/flatten (cdr x) acc)))))





#|
#|EXPORT|#				:make-id
 |#
(defun make-id (prefix &rest args)
  (let ((name (with-output-to-string (stream)
				(dolist (itm (cons prefix args))
				  (format stream "~A" (if (stringp itm)
										  (string-upcase itm) itm))))))
	(intern name 'keyword)))

#|
#|EXPORT|#				:fix-name
 |#
(defun fix-name (name &optional no-multiline)
  (setf name (if (symbolp name)
				 (string-downcase (symbol-name name)) name))
  (unless (stringp name)
	(error "fix-name : invalid data type."))
  (if no-multiline
	  name
	  (string/replace name "~%" "
")))



(defun __write-imp (stream itm)
  (when itm
	(if (keywordp itm)
		(princ (string-downcase (symbol-name itm)) stream)
		(if (and (numberp itm) (not (integerp itm)))
			(format stream "~F" (coerce itm 'single-float))
			(format stream "~A" itm)))))


#|
#|EXPORT|#				:exception
#|EXPORT|#				:caution
#|EXPORT|#				:throw-exception
#|EXPORT|#				:throw-caution
#|EXPORT|#				:type-assert
#|EXPORT|#				:chk-type
 |#
(define-condition exception (cl:error)
  ((msg :initarg  :msg :accessor exception-msg))
  (:report (lambda (condition stream)
			 (write-string (exception-msg condition) stream))))

(define-condition caution (cl:warning)
  ((msg :initarg  :msg :accessor caution-msg))
  (:report (lambda (condition stream)
			 (write-string (caution-msg condition) stream))))

(defmacro throw-exception (fmt &rest args)
  `(error (make-condition 'exception :msg (format nil ,fmt ,@args))))

(defmacro throw-caution (fmt &rest args)
  `(warn (make-condition 'caution :msg (format nil ,fmt ,@args))))

(defmacro type-assert (symbol type)
  `(check-type ,symbol ,type))

(defmacro chk-type (symbol type &optional (name nil))
  (let ((fmt (format nil "The value of ~A is ~~A, which is not of type ~A." (or name symbol) type)))
	`(unless (typep ,symbol ',type)
	   (throw-exception ,fmt ,symbol))))





#|
#|EXPORT|#				:format-string
 |#
(defun format-string (&rest args)
  (with-output-to-string (stream)
	(dolist (itm args)
	  (__write-imp stream itm))))

;;#|
;;#|EXPORT|#				:fix-member-when-nil
;; |#
;;(defmacro fix-member-when-nil (member value &optional (base nil) (kwd nil))
;;  (type-assert member symbol)
;;  (if (null base)
;;	  `(unless ,member (setf ,member ,value))
;;	  (progn
;;		(type-assert base symbol)
;;		(type-assert kwd  symbol)
;;		`(unless ,member
;;		   (setf ,member (or (and ,base (class:member ,base ,kwd)) ,value))))))

#|
#|EXPORT|#				:check-member
#|EXPORT|#				:check-object
#|EXPORT|#				:check-keywords
#|EXPORT|#				:check-numbers
 |#
(defmacro check-member (sym &key (nullable nil) (types nil))
  (type-assert sym symbol)
  (if (not nullable)
	  `(if (null ,sym)
		   (throw-exception ,(format nil "~A is nil." sym))
		   (chk-type ,sym ,types))
	  `(when ,sym
		 (chk-type ,sym ,types))))

(defmacro check-object (sym canvas dict &key (nullable nil) (class nil))
  (type-assert sym symbol)
  (type-assert canvas symbol)
  (type-assert dict   symbol)
  (type-assert class  symbol)
  `(,@(if nullable `(when ,sym) `(progn))
	  (unless (typep ,sym ',class)
		(throw-exception ,(format nil "parameter ~A is not ~A object." sym class)))
	  (check ,sym ,canvas ,dict)))

(defmacro check-keywords (sym &rest choices)
  (type-assert sym symbol)
  `(unless (or ,@(mapcar (lambda (v)
						   (type-assert v keyword)
						   `(eq ,sym ,v)) choices))
	 (throw-exception ,(format nil "~A must be in ~A." sym choices))))

(defmacro check-numbers (sym &rest choices)
  (type-assert sym symbol)
  `(unless (or ,@(mapcar (lambda (v)
						   (type-assert v number)
						   `(= ,sym ,v)) choices))
	 (throw-exception ,(format nil "~A must be in ~A." sym choices))))

#|
#|EXPORT|#				:write-when
#|EXPORT|#				:it
 |#
(defmacro write-when (item &rest args)
  `(let ((it ,item))
	 (when it
	   (format-string ,@args))))


#|
#|EXPORT|#				:with-dictionary
#|EXPORT|#				:attr
 |#
(defmacro with-dictionary (dict &rest body)
  (type-assert dict symbol)
  (labels ((property-ref-symbolp (x)
			 (when (and (symbolp x) (not (keywordp x)))
			   (let* ((name (symbol-name x))
					  (pos (position #\. name)))
				 (when pos
				   (and (< 0 pos)
						(< pos (1- (length name))))))))
		   (make-symbol-macrolet (sym)
			 (let* ((name (symbol-name sym))
					(pos  (position #\. name))
					(id   (subseq name 0 pos)))
			   (if (string= id "CANVAS")
				   (let ((id-sym (onlisp/symb (subseq name 0 pos)))
						 (method (onlisp/symb "CANVAS-DICT-" (subseq name (1+ pos)))))
					 `(,sym (,method ,id-sym)))
				   (let ((id-kwd (onlisp/keysymb (subseq name 0 pos)))
						 (method (onlisp/symb "SHAPE-" (subseq name (1+ pos)))))
					 `(,sym (,method (dict-get-entity ,dict ,id-kwd))))))))
	(let ((syms (remove-duplicates
				 (remove-if-not #'property-ref-symbolp
								(diagram::onlisp/flatten body)))))
	  `(symbol-macrolet ,(mapcar #'make-symbol-macrolet syms)
		 (labels ((get-dictionary () ,dict))
		   (declare (ignorable #'get-dictionary))
		   (macrolet ((attr (id name)
						(let ((func-sym (diagram::onlisp/symb "SHAPE-" name)))
						  (list ,'func-sym (list 'diagram::dict-get-entity ',dict id)))))
			 ,@body))))))

#|
#|EXPORT|#				:escape-characters
 |#
(defun escape-characters (str)
  (setf str (string/replace str "&"  "&amp;"))
  (setf str (string/replace str "<"  "&lt;"))
  (setf str (string/replace str ">"  "&gt;"))
  (setf str (string/replace str "\"" "&quot;"))
  (setf str (string/replace str "'"  "&#x27;"))
  str)


#|
#|EXPORT|#				:to-property-strings
#|EXPORT|#				:to-style-strings
 |#
(defgeneric to-property-strings (info))	;; ToDo : 他の場所に移動する？
(defgeneric to-style-strings (info))	;; ToDo : 他の場所に移動する？

#|
(let ((lnk (make-link "http://www.google.co.jp/"))
	  (writer (make-instance 'buffer-writer)))
  (write-link-open lnk writer)
  (writer-write writer "test")
  (write-link-close lnk writer)
  (writer-close writer))
|#

#|
#|EXPORT|#				:check
 |#
(defgeneric check (obj canvas dict))


#|
#|EXPORT|#				:rgb
 |#
(defun rgb (r g b)
  (labels ((fix (n)
			 (let ((n (if (floatp n)
						  (round (* 255 n)) n)))
			   (if (not (numberp n))
				   0
				   (if (< 255 n) 255 n)))))
	(format nil "#~2,'0x~2,'0x~2,'0x" (fix r) (fix g) (fix b))))
