#|
#|ASD|#				(:file "image"                     :depends-on ("cl-diagram"
#|ASD|#																"binutil"
#|ASD|#																"shape"
#|ASD|#																"label-info"
#|ASD|#																"link-info"
#|ASD|#																"point"
#|ASD|#																"writer"))
#|EXPORT|#				;image.lisp
 |#


(in-package :cl-diagram)

;-------------------------------------------------------------------------------
;
; internal uutilities
;
;-------------------------------------------------------------------------------
(defun __get-size-of-imagefile (file-name)
  (let ((img-type (string-downcase (pathname-type file-name))))
	(cond
	  ((string= img-type "png") ;----------------------------------------
	   (bin/with-read-stream (reader file-name)
		 (bin/seek-relative reader 16)
		 (values (bin/read-value :uint32 reader :endian :big)
				 (bin/read-value :uint32 reader :endian :big))))

	  ((or (string= img-type "jpg") ;------------------------------------
		   (string= img-type "jpeg"))
	   (bin/with-read-stream (reader file-name)
		 (bin/seek-relative reader 2) ; skip SOI mark
		 (let ((mark (bin/read-value :uint16 reader :endian :big))
			   (len  (bin/read-value :uint16 reader :endian :big)))
		   (do ()
			   ((= mark #xFFC0) nil)
			 (bin/seek-relative reader (- len 2))
			 (setf mark (bin/read-value :uint16 reader :endian :big))
			 (setf len  (bin/read-value :uint16 reader :endian :big)))
		   (bin/seek-relative reader 1)
		   (let ((height (bin/read-value :uint16 reader :endian :big))
				 (width  (bin/read-value :uint16 reader :endian :big)))
			 (values width height)))))

	  ((string= img-type "gif") ;----------------------------------------
	   (bin/with-read-stream (reader file-name)
		 (bin/seek-relative reader 6)
		 (values (bin/read-value :uint16 reader :endian :little)
				 (bin/read-value :uint16 reader :endian :little))))

	  ((string= img-type "bmp") ;----------------------------------------
	   (bin/with-read-stream (reader file-name)
		 ;; skip BITMAPFILEHEADER.
		 (bin/seek-relative reader 14)
		 ;; load bcSize ( 40 : windows bitmap, 12 : OS/2 bitmap )
		 (let ((bc-size (bin/read-value :uint32 reader :endian :little)))
		   (if (= bc-size 40)
			   (values (bin/read-value :uint32 reader :endian :little)
					   (bin/read-value :uint32 reader :endian :little))
			   (if (= bc-size 12)
				   (values (bin/read-value :uint16 reader :endian :little)
						   (bin/read-value :uint16 reader :endian :little)))))))

	  (t (values nil nil)))))
	   


;-------------------------------------------------------------------------------
;
; shape image
;
;-------------------------------------------------------------------------------
(defclass image (shape)
  ((center-x		;:type     number
					:initform 0
					:initarg  :center-x
					:accessor image-x)
   (center-y		;:type     number
					:initform 0
					:initarg  :center-y
					:accessor image-y)
   (filename		;:type     (or string pathname)
					:initform nil
					:initarg  :filename
					:accessor image-filename)
   (width			;:type     number
					:initform 0
					:initarg  :width
					:accessor image-width)
   (height			;:type     number
					:initform 0
					:initarg  :height
					:accessor image-height)
   (label			;:type     (or nil label-info)
					:initform nil
					:initarg  :label
					:accessor image-label)
   (preserve-ratio	;:type     boolean
					:initform t
					:initarg  :preserve-ratio
					:accessor image-preserve-ratio)))


(defmethod initialize-instance :after ((img image) &rest initargs)
  (declare (ignore initargs))
  (with-slots (label) img
	(when label
	  (setf label (make-label label))))
  img)


(defmethod check ((img image) canvas dict)
  ;; this method must call super class' one.
  (call-next-method)
  (check-member (center-x (image-x        img)) :nullable nil :types number)
  (check-member (center-y (image-y        img)) :nullable nil :types number)
  (check-member (filename (image-filename img)) :nullable nil :types (or pathname string))
  (check-member (width    (image-width    img)) :nullable   t :types number)
  (check-member (height   (image-height   img)) :nullable   t :types number)
  (with-slots (width height filename preserve-ratio) img
	(let ((path (merge-pathnames filename (path/get-current-directory))))
	  ;; 指定された名前のファイルがカレントディレクトリに存在することをチェック
	  (unless (path/is-existing-file path)
		(throw-exception "image file '~A' is not exist." path))
	  ;; width, height の両方とも明示的に指定されている場合、アスペクト比の維持をしない
	  (if (and width height)
		  (setf preserve-ratio nil)
		  ;; 上記以外の場合、画像ファイルの実際のサイズを取得する。
		  (multiple-value-bind (w h) (__get-size-of-imagefile path)
			;; width, height の両方とも nil ならば実際のサイズを設定
			(if (and (null width) (null height))
				(setf width  w
					  height h)
				;; 上記以外の場合、実サイズのアスペクト比にあわせて nil 側を設定
				(if (null width)
					(setf width  (* height (/ w h)))
					(setf height (* width  (/ h w)))))))))
  (check-member (width    (image-width    img)) :nullable nil :types number)
  (check-member (height   (image-height   img)) :nullable nil :types number)
  (check-object (label    (image-label    img)) canvas dict :nullable t :class label-info)
  (incf (image-x img) (canvas-left canvas))
  (incf (image-y img) (canvas-top  canvas))
  nil)


(defmethod shape-width ((img image))
  (image-width img))

(defmethod shape-height ((img image))
  (image-height img))

(defmethod shape-top ((img image))
  (- (image-y img)
	 (/ (image-height img) 2)))

(defmethod shape-middle ((img image))
  (image-y img))

(defmethod shape-bottom ((img image))
  (+ (image-y img)
	 (/ (image-height img) 2)))

(defmethod shape-left ((img image))
  (- (image-x img)
	 (/ (image-width img) 2)))

(defmethod shape-center ((img image))
  (image-x img))

(defmethod shape-right ((img image))
  (+ (image-x img)
	 (/ (image-width img) 2)))

;;MEMO : use impelementation of shape...
;;(defmethod shape-connect-point ((img image) type arg) ...)
  
;;MEMO : use impelementation of shape...
;;(defmethod shape-get-subcanvas ((img image)) ...)

(defmethod entity-composition-p ((img image))
  (if (image-label img)
	  t
	  (call-next-method)))

(defmethod draw-entity ((img image) writer)
  (let ((cls  (shape-class img))
		(id   (and (not (entity-composition-p img))
				   (entity-id img))))
	(pre-draw img writer)
	(writer-write writer
				  "<image "
				  (write-when id "id='" it "' ")
				  "x='" (shape-left img) "' "
				  "y='" (shape-top  img) "' "
				  "width='"  (shape-width  img) "' "
				  "height='" (shape-height img) "' "
				  (write-when cls "class='" it "' ")
				  "xlink:href='" (image-filename img) "' "
				  (unless (image-preserve-ratio img)
					"preserveAspectRatio='none' ")
				  "/>")
	(with-slots (label) img
	  (when label
		(draw-label label img writer)))
	(post-draw img writer))
  nil)


#|
#|EXPORT|#				:image
 |#
(defmacro image (x y filename
				 &key width height label class link layer id contents)
  (let ((code `(register-entity (make-instance 'diagram:image
											   :center-x ,x :center-y ,y
											   :filename ,filename
											   :width ,width :height ,height
											   :label ,label :class ,class
											   :link ,link :layer ,layer :id ,id))))
	(if (null contents)
		code
		(let ((g-obj (gensym "OBJ")))
		  `(let* ((,g-obj ,code)
				  (canvas (diagram:shape-get-subcanvas ,g-obj)))
			 (declare (special canvas))
			 ,@contents)))))

