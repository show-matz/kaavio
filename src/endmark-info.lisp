#|
#|ASD|#                (:file "endmark-info"              :depends-on ("kaavio"
#|ASD|#                                                                "constants"
#|ASD|#                                                                "mathutil"
#|ASD|#                                                                "point"
#|ASD|#                                                                "canvas"
#|ASD|#                                                                "dictionary"
#|ASD|#                                                                "fill-info"
#|ASD|#                                                                "stroke-info"
#|ASD|#                                                                "writer"))
#|EXPORT|#                ;endmark-info.lisp
 |#


(in-package :kaavio)


;;------------------------------------------------------------------------------
;;
;; internal functions
;;
;;------------------------------------------------------------------------------
(defun __draw-endmark-arrow (points size stroke fill clip-path writer)
  (declare (ignore fill))
  (symbol-macrolet ((ARROW_DEGREE1 210)
                    (ARROW_DEGREE2 150))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
           (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
           (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
           (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
      (writer-write writer
                    "<polyline "
                    "fill='none' "
                    (when stroke
                      (to-property-strings stroke))
                    (write-when clip-path "clip-path='url(#" it ")' ")
                    "points='"  (+ (point-x pt2) x2) "," (+ (point-y pt2) y2) " "
                                (point-x pt2)        "," (point-y pt2)        " "
                                (+ (point-x pt2) x1) "," (+ (point-y pt2) y1) "' "
                    "/>"))))

(defun __draw-endmark-triangle (points size stroke fill clip-path writer)
  (symbol-macrolet ((ARROW_DEGREE1 205)
                    (ARROW_DEGREE2 155))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1)))
           (y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1)))
           (x2 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2)))
           (y2 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
      (writer-write writer
                    "<path "
                    (when fill
                      (to-property-strings fill))
                    (when stroke
                      (to-property-strings stroke))
                    (write-when clip-path "clip-path='url(#" it ")' ")
                    "d='M " (point-x pt2) " " (point-y pt2) " "
                       "l " x1            " " y1            " "
                       "l " (- x2 x1)     " " (- y2 y1)     " z' "
                    "/>"))))

(defun __draw-endmark-diamond (points size stroke fill clip-path writer)
  (symbol-macrolet ((ARROW_DEGREE1 210)
                    (ARROW_DEGREE2 150))
    (let* ((pt1 (car points))
           (pt2 (cdr points))
           (x1 (point-x pt2))
           (y1 (point-y pt2))
           (x2 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE1))))
           (y2 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE1))))
           (x4 (+ x1 (* size (math/cos3 pt1 pt2 ARROW_DEGREE2))))
           (y4 (+ y1 (* size (math/sin3 pt1 pt2 ARROW_DEGREE2))))
           (x3 (+ x2 (- x4 x1)))
           (y3 (+ y2 (- y4 y1))))
      (writer-write writer
                    "<path "
                    (when fill
                      (to-property-strings fill))
                    (when stroke
                      (to-property-strings stroke))
                    (write-when clip-path "clip-path='url(#" it ")' ")
                    "d='M " x1 " " y1 " L " x2 " " y2 " "
                       "L " x3 " " y3 " L " x4 " " y4 " z' "
                    "/>"))))

(defun __draw-endmark-circle (points size stroke fill clip-path writer)
  (let ((pt (cdr points)))
    (writer-write writer
                  "<circle "
                  "cx='" (point-x pt) "' "
                  "cy='" (point-y pt) "' "
                  "r='" (/ size 2) "' "
                  (when fill
                    (to-property-strings fill))
                  (when stroke
                    (to-property-strings stroke))
                  (write-when clip-path "clip-path='url(#" it ")' ")
                  "/>")))

(defun __draw-endmark-rectangle (points size stroke fill clip-path writer)
  (let* ((pt1  (car points))
         (pt2  (cdr points))
         (r    (/ size (sqrt 2)))
         (cx   (point-x pt2))
         (cy   (point-y pt2)))
    (writer-write writer
                  "<path "
                  (when fill
                    (to-property-strings fill))
                  (when stroke
                    (to-property-strings stroke))
                  (write-when clip-path "clip-path='url(#" it ")' ")
                  "d='M " (+ cx (* r (math/cos3 pt1 pt2  45)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2  45)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 135)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 135)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 225)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 225)))
                  " L "   (+ cx (* r (math/cos3 pt1 pt2 315)))
                  " "     (+ cy (* r (math/sin3 pt1 pt2 315))) " z' />")))

;;------------------------------------------------------------------------------
;;
;; class endmark-info
;;
;;------------------------------------------------------------------------------
#|
#|EXPORT|#                :endmark-info
 |#
(defclass endmark-info ()
  ((type    :initform nil :initarg :type)       ; (or keyword function)
                                                ; :none|:arrow|:triangle|:diamond|:circle|:rect
   (size    :initform nil :initarg :size)       ; (or keyword number)
                                                ; :small|:medium|:large|:xlarge
   (fill    :initform nil :initarg :fill)       ; (or nil fill-info)    ; nil means same as stroke
   (stroke  :initform nil :initarg :stroke)))   ; (or nil stroke-info)


(defmethod initialize-instance :after ((mark endmark-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (type size fill stroke) mark
    (setf type   (or type *default-endmark-type*))
    (setf size   (or size *default-endmark-size*))
    (when (or fill *default-endmark-fill*)
      (setf fill (make-fill (or fill *default-endmark-fill*))))
    (when stroke
      (setf stroke (make-stroke stroke))))
  mark)


(defmethod check ((mark endmark-info) canvas dict)
  (with-slots (type size fill stroke) mark
    (check-member type   :nullable nil :types (or keyword function))
    (check-member size   :nullable nil :types (or keyword number))
    (check-object fill   canvas dict :nullable t :class   fill-info)
    (check-object stroke canvas dict :nullable t :class stroke-info)
    (when (keywordp type)
      (check-keywords type :arrow :triangle :diamond :circle :rect))
    (when (keywordp size)
      (check-keywords size :small :medium :large :xlarge)))
  t)

(defun draw-endmark (mark points stroke clip-path writer)
  ;; RULE : 1. mark 自体に stroke/fill が設定されていればそれを使う
  ;;        2. stroke が mark に設定されていない場合、パラメータの stroke を使う
  ;;        3. fill が mark に設定されていない場合、stroke の color/url を使う
  (with-slots (type size fill) mark
    (let* ((sz (if (numberp size)
                   size
                   (ecase size
                     ((:small)  10.0)
                     ((:medium) 15.0)
                     ((:large)  20.0)
                     ((:xlarge) 30.0))))
           (st (or (slot-value mark 'stroke)
                   (make-stroke :dasharray nil :base stroke)))
           (fl (or fill (make-fill :color   (slot-value st 'color)
                                   :url     (slot-value st 'url)
                                   :opacity (slot-value st 'opacity))))
           (drawer (if (functionp type)
                       type
                       (ecase type
                         ((:arrow)    #'__draw-endmark-arrow)
                         ((:triangle) #'__draw-endmark-triangle)
                         ((:diamond)  #'__draw-endmark-diamond)
                         ((:circle)   #'__draw-endmark-circle)
                         ((:rect)     #'__draw-endmark-rectangle)))))
      (funcall drawer points sz st fl clip-path writer))))
  

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### function make-endmark
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{make-endmark}} ${REST} params
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　指定されたパラメータで終端マーク情報を生成します。終端マーク情報の詳細は「[](#終端マーク)」を
;;参照してください。上記は簡潔な記述で柔軟な終端マーク情報の生成を可能にするためのもので、 
;;`params` として渡されるパラメータ数に応じて以下のことをします。
;;
;;* パラメータ数が 0 の場合
;;    * nil を返します
;;* パラメータ数が 1 の場合
;;    * 終端マーク情報が渡された場合、それをそのまま返します
;;    * 数値 N が渡された場合、 `(make-endmark :size N)` を返します
;;    * リスト lst が渡された場合、 `(apply #'make-endmark lst)` を返します
;;    * 上記のいずれでもない prm の場合、 `(make-endmark :type prm)` を返します
;;* パラメータ数が 2 以上の場合
;;    * 後述します
;;
;;　パラメータ数が 2 以上の場合、make-endmark 関数は実質的に以下の関数であるかのように振舞います。
;;
;;<!-- stack:push li class='syntax' -->
;;
;;* ${{B}{make-endmark}} ${KEY} type size stroke fill
;;
;;<!-- stack:pop li -->
;;
;;　各パラメータの意味は以下の通りです。詳細は「[](#終端マーク)」を参照してください。
;;
;;* `type` ---- 終端マークの形状を指定します。 `:arrow :triangle :diamond :circle :rect` のいずれか、またはカスタム描画関数を指定します。
;;* `size` ---- 終端マークのサイズを指定します。 `:small :medium :large :xlarge` のいずれか、または数値を指定します。
;;* `stroke` ---- 終端マークを描画するストロークを指定します。省略した場合、実際に終端マークが描画される対象（直線やコネクタ）のストロークが使用されます。ただしその場合、そのストロークの `:dasharray` など一部のパラメータは引き継がれません。
;;* `fill` ---- 終端マーク内部の塗り潰しを指定します。省略した場合、ストロークと同じ色で塗り潰されます。
;;
;;${SEE_ALSO}
;;
;;* [](#終端マーク)
;;
;;${NOTES}
;;
;;　`type` パラメータでキーワードのかわりに指定するカスタム描画関数ですが、以下のシグネチャの
;;関数を指定する必要があります。これは現状では暫定的で undocumented な状態です。詳細はコードを
;;参照してください。
;;
;;```lisp
;;(lambda (points size stroke fill writer) ...)
;;```
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :make-endmark
 |#
(defun make-endmark (&rest params)
  (if (= 1 (length params))
      (let ((param (car params)))
        (cond
          ((typep param 'endmark-info) param)
          ((keywordp param) (make-endmark :type param))
          ((numberp  param) (make-endmark :size param))
          ((listp    param) (apply #'make-endmark param))
          (t                (make-endmark :type param))))
      (if (null params)
          nil
          (destructuring-bind (&key type size fill stroke) params
            (make-instance 'endmark-info
                           :type   type
                           :size   size
                           :fill   fill
                           :stroke stroke)))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### macro with-endmark-options
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{with-endmark-options}} (${KEY} type size fill end1 end2) ${BODY} body
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　終端マークのデフォルトオプションを変更します。キーワードパラメータ群の説明は 
;;[$@ 節](#終端マーク)を参照してください。
;;
;;${SEE_ALSO}
;;
;;* [](#終端マーク)
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|EXPORT|#                :with-endmark-options
 |#
(defmacro with-endmark-options ((&key type size fill end1 end2) &rest body)
  (labels ((impl (params acc)
             (if (null params)
                 acc
                 (let ((value  (car  params))
                       (symbol (cadr params)))
                   (impl (cddr params)
                         (if (null value)
                             acc
                             (push (list symbol value) acc)))))))
    (let ((lst (impl (list type '*default-endmark-type*
                           size '*default-endmark-size*
                           fill '*default-endmark-fill*
                           end1 '*default-endmark-1*
                           end2 '*default-endmark-2*) nil)))
      `(let ,lst
         ,@body))))
