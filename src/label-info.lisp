#|
#|ASD|#                (:file "label-info"                :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "font-info"
#|ASD|#                                                                "shape"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;label-info.lisp
 |#

(in-package :kaavio)

;;------------------------------------------------------------------------------
;;
;; class label-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :label-info
 |#
(defclass label-info ()
  ((text     :initform  "" :initarg :text)      ; (or keyword string)
   (position :initform nil :initarg :position)  ; keyword - :above :below :left :right
   (offset   :initform nil :initarg :offset)    ; (or nil list)
   (font     :initform nil :initarg :font)))    ; font-info


(defmethod initialize-instance :after ((label label-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (position font offset) label
    (setf position (or position *default-label-position*))
    (setf   offset (or offset   *default-label-offset* (make-point 0 0)))
    (setf     font (make-font (or font *default-label-font* *default-font*))))
  label)


(defmethod check ((ent label-info) canvas dict)
  (with-slots (text position offset font) ent
    (check-member text     :nullable nil :types (or keyword string))
    (check-member position :nullable nil :types keyword)
    (check-member offset   :nullable   t :types cons)
    (when offset
      (with-point (x y) offset
        (check-member x    :nullable nil :types number)
        (check-member y    :nullable nil :types number)))
    (check-object font     canvas dict :nullable nil :class font-info)
    (check-keywords position :above :below :left :right))
  t)

(defun label-info-locate-text-for-above (shp offset lines font-size spacing)
  (let* ((pt     (attribute-top shp))
         (cnt    (length lines))
         (height (+ (* (1- cnt) font-size) (* cnt spacing))))
    (values :center
            (point+ (point/y+ pt (- height)) offset))))

(defun label-info-locate-text-for-below (shp offset lines font-size spacing)
  (declare (ignore lines))
  (let* ((pt (attribute-bottom shp)))
    (values :center
            (point+ (point/y+ pt (+ spacing font-size)) offset))))

(defun label-info-locate-text-for-left (shp offset lines font-size spacing)
  (let* ((pt     (attribute-left shp))
         (cnt    (length lines))
         (height (+ (* cnt font-size) (* (1- cnt) spacing))))
    (values :right
            (point+ (point/y+ pt (- font-size (/ height 2))) offset))))

(defun label-info-locate-text-for-right (shp offset lines font-size spacing)
  (let* ((pt     (attribute-right shp))
         (cnt    (length lines))
         (height (+ (* cnt font-size) (* (1- cnt) spacing))))
    (values :left
            (point+ (point/y+ pt (- font-size (/ height 2))) offset))))
  
  
#|
#|EXPORT|#                :draw-label-with-point
 |#
(defun draw-label-with-point (label x y sin cos clip-path writer)
  (with-slots (text offset font) label
    (labels ((calc-width-and-height ()
               (let* ((width     0)
                      (height    0)
                      (lines     (fix-name text))
                      (font-size (slot-value font 'kaavio::size))
                      (spacing   (slot-value font 'kaavio::line-spacing)))
                 (multiple-value-setq (width height)
                                      (font-calc-textarea font lines))
                 (values width height font-size
                         spacing (string/split lines #\newline)))))
      (multiple-value-bind (width height
                            font-size spacing lines) (calc-width-and-height)
        ;(format t "x:~A, y:~A, sin:~A, cos:~A, width=~A, height~A~%" x y sin cos width height)
        (let ((x (+ x (* sin (/ height 2))))
              (y (- y (* cos (/ height 2)))))
          (decf y (/ height 2))
          (when (< 0 sin) (incf x (/ width 2)))
          (when (< sin 0) (decf x (/ width 2)))
          (dolist (line lines)
            (incf y font-size)
            (write-text-tag (+ x (point-x offset))
                            (+ y (point-y offset))
                            line writer :align :center :font font :clip-path clip-path)
            (incf y spacing)))))))

#|
#|EXPORT|#                :draw-label
 |#
(defun draw-label (label shp clip-path writer)
  (with-slots (text offset position font) label
    (let ((size    (slot-value font 'size))
          (spacing (slot-value font 'line-spacing))
          (lines   (string/split (fix-name text) #\newline)))
      (unless (typep shp 'shape)
        (throw-exception "label-info : shp is not type of shape."))
      (labels ((get-location-info ()
                 (let ((locater (ecase position
                                  ((:above) #'label-info-locate-text-for-above)
                                  ((:below) #'label-info-locate-text-for-below)
                                  ((:left)  #'label-info-locate-text-for-left )
                                  ((:right) #'label-info-locate-text-for-right))))
                   (funcall locater shp offset lines size spacing))))
        (multiple-value-bind (align pt) (get-location-info)
          (dolist (line lines)
            (write-text-tag (point-x pt)
                            (point-y pt) line writer :align align :font font :clip-path clip-path)
            (incf (point-y pt) (+ spacing size))))))))
  
  
;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-label
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-label}} ${REST} params
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　指定されたパラメータでラベル情報を生成します。ラベル情報の詳細は「[](#ラベル)」を参照して
;;ください。上記は簡潔な記述で柔軟なラベル情報の生成を可能にするためのもので、 `params` として
;;渡されるパラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * nil を返します
;;* パラメータ数が 1 の場合
;;    * ラベル情報が渡された場合、それをそのまま返します
;;    * リスト lst が渡された場合、 `(apply #'make-label lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-label prm :offset nil)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-label 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-label}} text ${KEY} position offset font
;;
;;<!-- stack:pop li -->
;;
;;　各パラメータの意味は以下の通りです。詳細は「[](#ラベル)」を参照してください。
;;
;;* `text` ---- ラベルのテキストを文字列かキーワードシンボルで指定します。
;;* `position` ---- ラベルを表示する位置を `:above :below :left :right` のいずれかで指定します。直線やコネクタでは無視されます。
;;* `offset` ---- ラベルの表示位置を微調整する `(x y)` 値を指定します。
;;* `font` ---- ラベルの描画に使用するフォントを指定します。
;;
;;${SEE_ALSO}
;;
;;* [](#ラベル)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :make-label
 |#
(defun make-label (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'label-info) param)
          ((keywordp param) (make-label param :offset nil))
          ((stringp  param) (make-label param :offset nil))
          ((listp    param) (apply #'make-label param))
          (t                (make-label param :offset nil))))
      (if (null params)
          nil
          (destructuring-bind (text &key position offset font) params
            (make-instance 'label-info
                           :text     text   :position position
                           :offset   offset :font     font)))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-label-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-label-options}} (${KEY} position offset font) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ラベルのデフォルトオプションを変更します。キーワードパラメータ群の説明は [$@ 節](#ラベル)を
;;参照してください。
;;
;;${SEE_ALSO}
;;
;;* [](#ラベル)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-label-options
 |#
(defmacro with-label-options ((&key (position nil position-p)
                                    (offset   nil offset-p)
                                    (font     nil font-p)) &rest body)
  (let ((bindings nil))
    (labels ((impl (arg-p binding)
               (when arg-p (push binding bindings))))
      (impl position-p `(*default-label-position* ,position))
      (impl offset-p   `(*default-label-offset*   ,offset))
      (impl font-p     `(*default-label-font*     (make-font2 *default-label-font* ,font))))
    `(let ,(nreverse bindings)
       ,@body)))
